library(shiny)
library(bs4Dash)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tibble)
library(stringr)
library(googlesheets4)
library(patchwork)
library(tidyr)
library(DT)
library(janitor)

# Setup ----------------------------------------------------------------------

## Read in time entries, create totals tables ----------------------------

# time entries being written out every minute onto the server

latest_entries_file <- list.files("/home/daniel/toggl-entries") %>% 
  as_tibble() %>% 
  arrange(desc(value)) %>% 
  slice(1) %>% 
  pull()

entries_df <- read_csv(str_c("/home/daniel/toggl-entries/", latest_entries_file)) %>% 
  mutate(date = lubridate::date(start))

# FOR LOCAL TESTING ONLY, DON'T PUSH THESE FILE PATHS

#latest_entries_file <- list.files(here::here("toggl-entries")) %>% 
#  as_tibble() %>% 
#  arrange(desc(value)) %>% 
#  slice(1) %>% 
#  pull()

#entries_df <- read_csv(here::here("toggl-entries", latest_entries_file)) %>% 
#  mutate(date = lubridate::date(start))


#gs4_auth("dgdulaney1@gmail.com")

#entries_df <- read_sheet("1HAx_fVyI134wue7rroh_FaLh9DxlIQK8VQiNyD8m5cQ", sheet = "Sheet1") %>% 
#  mutate(date = lubridate::date(start))

#entries_df <- read_csv("https://raw.githubusercontent.com/ddstats1/time-tracking/master/tracker/time_entries-2023-05-05.csv") %>% 
#  mutate(date = lubridate::date(start))




# get df's with daily/weekly/monthly/yearly totals by project
daily_totals <- entries_df %>% 
  group_by(date, project_name) %>% 
  summarise(secs = sum(duration)) %>% 
  ungroup()

weekly_totals <- entries_df %>% 
  group_by(week = cut(date, "week"), project_name) %>% 
  summarise(secs = sum(duration)) %>% 
  ungroup() %>% 
  mutate(week = as.Date(week))

monthly_totals <- entries_df %>% 
  group_by(week = cut(date, "month"), project_name) %>% 
  summarise(secs = sum(duration)) %>% 
  ungroup()



## Read in daily goals table, create weekly/monthly tables ----------------------------

daily_goals_df_raw <- read_csv("https://raw.githubusercontent.com/ddstats1/time-tracking/master/tracker/time_goals_2023-05-09.csv")

#daily_goals_df_raw <- read_sheet("1JEtORWEfRUv_6sBLzQzXKrR5Bvc3hs3njQSm6y6Khtk", 
#                                 sheet = "Daily Goals")

daily_goals_df <- daily_goals_df_raw %>% 
  pivot_longer(cols = BlueLabs:`time-with-family`, 
               names_to = "project_name", 
               values_to = "mins_goal") %>% 
  mutate(date = lubridate::date(date))

# create weekly and monthly df's based on the daily one -- to get week goals for
# BlueLabs at week of 04/24, grab that date + 6 more days BlueLabs goals totals
# and sum them!

weekly_goals_df <- daily_goals_df %>% 
  group_by(week = cut(date, "week"), project_name) %>% 
  summarise(mins_goal = sum(mins_goal, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(week = as.Date(week))

monthly_goals_df <- daily_goals_df %>% 
  group_by(month = cut(date, "month"), project_name) %>% 
  summarise(mins_goal = sum(mins_goal, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(month = as.Date(month))



## Helper functions ----------------------------

# to turn values in a column into clean_names() output
clean_some_names <- function(dat, idx, ...) {
  names(dat)[idx] <- janitor::make_clean_names(names(dat)[idx], ...)
  dat
}


plot_donut <- function(time_pd = c("day", "week", "month", "year"), project, date_) {
  
  if (time_pd == "day") {
    
    totals <- daily_goals_df %>% 
      left_join(daily_totals, by = c("date", "project_name")) %>% 
      filter(project_name == {{ project }}, 
             date == as.Date({{ date_ }})) %>% 
      mutate(mins_complete = secs / 60,
             pct_complete = mins_complete / mins_goal,
             # if missing, make 0
             mins_complete = ifelse(is.na(mins_complete), 0, mins_complete),
             mins_goal = ifelse(is.na(mins_goal), 0, mins_goal),
             pct_complete = ifelse(is.na(pct_complete), 0, pct_complete)) %>% 
      select(date, project_name, mins_complete, mins_goal, pct_complete)
    
  } else if (time_pd == "week") {
    
    totals <- weekly_goals_df %>% 
      left_join(weekly_totals, by = c("week", "project_name")) %>% 
      filter(project_name == {{ project }}, 
             week == as.Date({{ date_ }})) %>% 
      mutate(mins_complete = secs / 60,
             pct_complete = mins_complete / mins_goal,
             # if missing, make 0
             mins_complete = ifelse(is.na(mins_complete), 0, mins_complete),
             mins_goal = ifelse(is.na(mins_goal), 0, mins_goal),
             pct_complete = ifelse(is.na(pct_complete), 0, pct_complete)) %>% 
      select(week, project_name, mins_complete, mins_goal, pct_complete)
    
  } else if (time_pd == "month") {
    
    totals <- 999
    
  } else if (time_pd == "year") {
    
    totals <- 999
    
  }
  
  # Plot title, caption
  if (time_pd == "day") {
    
    plot_title = "Today"
    plot_caption = str_c(round(totals$mins_complete, 0), " / ", totals$mins_goal, "m")
    
  } else if (time_pd == "week") {
    
    plot_title = "Week"
    plot_caption = str_c(round(totals$mins_complete / 60, 0), " / ", totals$mins_goal / 60, "h")
    
  } else if (time_pd == "month") {
    
    plot_title = "Month"
    
  } else if (time_pd == "year") {
    
    plot_title = "Year"
    
  }
  
  
  # if task is not yet complete
  
  if ((totals$mins_goal == 0) | totals$mins_complete[1] < totals$mins_goal[1]) {
    
    plot_df <- tibble::tribble(
      ~cat, ~ymin, ~ymax,
      "goal", 0, totals$pct_complete[1],
      "rest", totals$pct_complete[1], 1
    )
    
    plot_df %>% 
      # rectangle of fixed width where x% is colored (pct done) and the rest is grey
      # displayed with % in middle and __ / __ mins below, just like on Hours
      ggplot(aes(ymin = ymin, ymax = ymax, xmin = 3, xmax = 4, fill = cat)) +
      geom_rect() +
      coord_polar(theta = "y", clip = "off") +
      # % value in middle of donut (95% done)
      annotate("text", x = 1.2, y = 1, size = 6, color = "#fac32a",
               label = str_c(signif(totals$pct_complete[1], 2) * 100, "%")) +
      # minutes/hours left
      #annotate("text", x = 1, y = 1, size = 5, color = "#fac32a",
      #         label = str_c(totals$mins, "m//", totals$mins_goal, "m")) +
      
      xlim(1, 4) +
      ylim(0, 1) +
      scale_fill_manual(values = c("#fac32a", "grey")) +
      # remove legend
      guides(fill = "none") +
      labs(title = plot_title,
           # 25/90m label at bottom of chart
           caption = plot_caption) +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5, vjust = -2.25, size = 20),
            plot.caption = element_text(hjust = 0.5, vjust = 25, size = 9, color = "grey2"))
  } 
  
  
  # else if task is complete
  
  else if (totals$mins_complete[1] >= totals$mins_goal[1]) {
    
    plot_df <- tibble::tribble(
      ~cat, ~ymin, ~ymax,
      "goal", 0, 1,
      "rest", 1, 1
    )
    
    plot_df %>% 
      # rectangle of fixed width where x% is colored (pct done) and the rest is grey
      # displayed with % in middle and __ / __ mins below, just like on Hours
      ggplot(aes(ymin = ymin, ymax = ymax, xmin = 3, xmax = 4, fill = cat)) +
      geom_rect() +
      coord_polar(theta = "y", clip = "off") +
      # % value in middle of donut (95% done)
      annotate("text", x = 1.2, y = 1, size = 5, color = "green3",
               label = str_c(signif(totals$pct_complete[1], 2) * 100, "%")) +
      # minutes/hours left
      #annotate("text", x = 1, y = 1, size = 5, color = "#fac32a",
      #         label = str_c(day_totals$mins, "m//", day_totals$mins_goal, "m")) +
      
      xlim(1, 4) +
      ylim(0, 1) +
      scale_fill_manual(values = c("green3", "grey")) +
      # remove legend
      guides(fill = "none") +
      labs(title = plot_title,
           caption = plot_caption) +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5, vjust = -2.25, size = 20),
            plot.caption = element_text(hjust = 0.5, vjust = 25, size = 9, color = "grey2"))
  }
}

# test
plot_donut("day", "BlueLabs", "2023-05-05")
plot_donut("week", "BlueLabs", "2023-05-01")

plot_donut("day", "pers-project", "2023-05-05")
plot_donut("week", "pers-project", "2023-05-01")

# top 5 tasks done for a specific project over the last week (and would be
# cool to be able to switch to last 2/3 weeks, last month, last 4 months, etc)
get_top_tasks <- function(project, start_date, end_date) {
  entries_df %>% 
    filter(project_name == {{ project }},
           date >= {{ start_date }},
           date <= {{ end_date }}) %>% 
    group_by(description) %>% 
    summarise(secs_total = sum(duration),
              mins_total = secs_total / 60,
              hrs_total = mins_total / 60) %>% 
    ungroup() %>% 
    arrange(desc(mins_total)) %>% 
    select(description, mins = mins_total, hrs = hrs_total) %>% 
    mutate(mins = round(mins, 1), 
           hrs = round(hrs, 1))
  # can slice if necessary, as well, if table gets too big. or maybe
  # have a rows-limit option in the app
}

# test


boxProject <- function(project) {
  
}




# UI ----------------------------------------------------------------------

ui <- dashboardPage(
  
  title = "Box API",
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Donut", tabName = "donut"),
      menuItem("Today", tabName = "today"),
      menuItem("Reports", tabName = "reports")
    )
  ),
  dashboardBody(
    
    # remove horizontal scrollbars under boxes
    tags$head(
      tags$style(HTML('.shiny-split-layout>div {overflow: hidden;}')),
    ),
    
    # so mobile version looks just like the desktop version
    tags$head( 
      tags$meta(name = "viewport", content = "width=1600"), 
      uiOutput("body")
    ),
    
    # add outline to boxes
    tags$script(HTML("$('.box').eq(0).css('border', '5px solid #3DA0D1');")),
    
    tabItems(
      
      ## Donut Page ----------------------------------
      
      tabItem(tabName = "donut",
              
              # FIRST ROW OF BOXES (3 total)
              
              splitLayout(
                # 3 boxes in the first row
                cellWidths = c("33.33%", "33.33%", "33.33%"),
                
                # BlueLabs donuts
                
                fluidRow(
                  box(
                    title = span("BlueLabs", icon("galactic-republic")), 
                    id = "bl_donuts", 
                    width = 12,
                    
                    # split into thirds -- one section for daily donut, one weekly, one monthly
                    splitLayout(
                      cellWidths = c("33.33%", "33.33%", "33.33%"),
                      plotOutput("plot_bl_donut_day", height = "200px", width = "175px"),
                      plotOutput("plot_bl_donut_week", height = "200px", width = "175px"),
                      plotOutput("plot_bl_donut_month", height = "200px", width = "175px")
                    )
                  )
                ),
                
                # Read Books donuts
                
                fluidRow(
                  box(
                    title = span("Read Books", icon("book")), 
                    id = "books_donuts", 
                    width = 12,
                    
                    # split into thirds -- one section for daily donut, one weekly, one monthly
                    splitLayout(
                      cellWidths = c("33.33%", "33.33%", "33.33%"),
                      plotOutput("plot_books_donut_day", height = "200px", width = "175px"),
                      plotOutput("plot_books_donut_week", height = "200px", width = "175px"),
                      plotOutput("plot_books_donut_month", height = "200px", width = "175px")
                    )
                  )
                ),
                
                # Pers Project donuts
                
                fluidRow(
                  box(
                    title = span("Pers Projects", icon("list-check")), 
                    id = "proj_donuts", 
                    width = 12,
                    
                    # split into thirds -- one section for daily donut, one weekly, one monthly
                    splitLayout(
                      cellWidths = c("33.33%", "33.33%", "33.33%"),
                      plotOutput("plot_proj_donut_day", height = "200px", width = "175px"),
                      plotOutput("plot_proj_donut_week", height = "200px", width = "175px"),
                      plotOutput("plot_proj_donut_month", height = "200px", width = "175px")
                    )
                  )
                )
              )
              
              # SECOND ROW OF BOXES (3 total)
              
      ),
      
      ## Today Page ----------------------------------
      
      # should only include tasks that have a goal for this day
      
      tabItem(tabName = "today",
              999),
      
      
      ## Reports Page ----------------------------------
      
      tabItem(tabName = "reports",
              
              fluidRow(
                box(
                  title = "BlueLabs", 
                  id = "bl_input", 
                  width = 12,
                  column(width = 3, 
                         dateInput("read-books", "Read Books"))
                )
              )
      )
    ),
    
    # no box header
    tags$head(tags$style('#foo .box-header{ display: none}'))
    
    # in the first tab, each box is a project-card with progress summaries (and
    # some other info? list of most done/recent tasks in that project?) on front
    # of card, and details found within clicks
    
    # the second tab will have trends for all projects to see how they're 
    # all working together
    
    # create a function that'll build a new project box?
    
    #boxProject()
    
  )
)


# Server ----------------------------------------------------------------------

server <- function(input, output, session) {
  
  ## Donut Page ----------------------------
  
  ## BlueLabs donuts
  
  output$plot_bl_donut_day <- renderPlot({
    plot_donut(time_pd = "day", 
               project = "BlueLabs", 
               date_ = Sys.Date()) 
  })
  
  output$plot_bl_donut_week <- renderPlot({
    # for week plots, go back to the Monday of this week
    plot_donut(time_pd = "week", 
               project = "BlueLabs", 
               date_ = lubridate::floor_date(Sys.Date(), "week", 1)) 
  })
  
  output$plot_bl_donut_month <- renderPlot({
    plot_donut(time_pd = "week", 
               project = "BlueLabs", 
               date_ = lubridate::floor_date(Sys.Date(), "week", 1))
  })
  
  ## Books donuts
  
  output$plot_books_donut_day <- renderPlot({
    plot_donut(time_pd = "day", 
               project = "read-books", 
               date_ = Sys.Date()) 
  })
  
  output$plot_books_donut_week <- renderPlot({
    plot_donut(time_pd = "week", 
               project = "read-books", 
               date_ = lubridate::floor_date(Sys.Date(), "week", 1)) 
  })
  
  output$plot_books_donut_month <- renderPlot({
    plot_donut(time_pd = "week", 
               project = "read-books", 
               date_ = lubridate::floor_date(Sys.Date(), "week", 1)) 
  })
  
  ## Pers Project donuts
  
  output$plot_proj_donut_day <- renderPlot({
    plot_donut(time_pd = "day", 
               project = "pers-project", 
               date_ = Sys.Date()) 
  })
  
  output$plot_proj_donut_week <- renderPlot({
    plot_donut(time_pd = "week", 
               project = "pers-project", 
               date_ = lubridate::floor_date(Sys.Date(), "week", 1)) 
  })
  
  output$plot_proj_donut_month <- renderPlot({
    plot_donut(time_pd = "week", 
               project = "pers-project", 
               date_ = lubridate::floor_date(Sys.Date(), "week", 1)) 
  })
  
  
  ## Today Page ----------------------------
  
  
  
  ## Reports Page ----------------------------
  
  output$top_tasks_bl <- DT::renderDataTable(
    get_top_tasks(project = "BlueLabs", start_date = input$bl_tasks_start, end_date = input$bl_tasks_end),
    options = list(pageLength = 10, lengthChange = FALSE, dom = "t", filter = list(position = "top")),
    rownames = FALSE,
    colnames = c("", "mins", "hrs")
  )
  
}

# so will deploy properly
#keyring::keyring_unlock("system", password = "2022")

# run app locally
shinyApp(ui, server)



# Setup ----------------------------------------------------------------------

#api <- togglr::set_toggl_api_token("5198c02c893360bb30c4cc1bf8b069ec")

#df <- togglr::get_time_entries(api, since = "2023-01-01")

#df %>% 
#write_csv(here::here("time-entries.csv"))