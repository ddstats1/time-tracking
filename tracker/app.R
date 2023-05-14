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

# for testing only -- the actual app only references the reactive versions
# of these, e.g. daily_totals(), weekly_totals(), ...

# entries_df <- read_csv(here::here("toggl-entries", "time_entries.csv")) %>%
#   mutate(start = start - lubridate::hours(7),
#          stop = stop - lubridate::hours(7),
#          date = lubridate::date(start))
# 
# daily_totals_df <- entries_df %>%
#   group_by(date, project_name) %>%
#   summarise(secs = sum(duration)) #%>%
# #ungroup()
# 
# weekly_totals_df <- entries_df %>%
#   group_by(week = cut(date, "week"), project_name) %>%
#   summarise(secs = sum(duration)) %>%
# #ungroup() %>%
#   mutate(week = as.Date(week))
# 
# monthly_totals_df <- entries_df %>%
#   group_by(month = cut(date, "month"), project_name) %>%
#   summarise(secs = sum(duration)) %>%
# #ungroup() %>%
#   mutate(month = as.Date(month))



## Read in daily goals table, create weekly/monthly tables ----------------------------

# daily_goals_df_raw <- read_csv("https://raw.githubusercontent.com/ddstats1/time-tracking/master/tracker/time_goals.csv")
# 
# daily_goals_df <- daily_goals_df_raw %>% 
#   pivot_longer(cols = 2:ncol(.), 
#                names_to = "project_name", 
#                values_to = "mins_goal") %>% 
#   mutate(date = lubridate::date(date),
#          # janky but makes blank ggplot() return work nice...
#          n_days_goal_entered = ifelse(mins_goal %in% c(NA, 0), 0, 1))
# 
# weekly_goals_df <- daily_goals_df %>% 
#   group_by(week = cut(date, "week"), project_name) %>% 
#   summarise(mins_goal = sum(mins_goal, na.rm = TRUE),
#             n_days_goal_entered = sum(n_days_goal_entered)) %>% 
#   #ungroup() %>% 
#   mutate(week = as.Date(week))
# 
# monthly_goals_df <- daily_goals_df %>% 
#   group_by(month = cut(date, "month"), project_name) %>% 
#   summarise(mins_goal = sum(mins_goal, na.rm = TRUE),
#             n_days_goal_entered = sum(n_days_goal_entered)) %>% 
#   #ungroup() %>% 
#   mutate(month = as.Date(month))



# then, task goals





## Functions ----------------------------

plot_donut <- function(time_pd = c("day", "week", "month", "year"), totals_df, goals_df, project, date_) {
  
  # TODO: make join_by column (day, week, or month) a variable based on time_pd, 
  # just have one "totals <-" pipe
  
  if (time_pd == "day") {
    
    # get table with mins completed vs mins goal for the day
    totals <- goals_df %>% 
      left_join(totals_df, by = c("date", "project_name")) %>% 
      filter(project_name == {{ project }}, 
             date == as.Date({{ date_ }})) %>% 
      mutate(mins_complete = secs / 60,
             pct_complete = mins_complete / mins_goal,
             # if missing, make 0
             mins_complete = ifelse(is.na(mins_complete), 0, mins_complete),
             mins_goal = ifelse(is.na(mins_goal), 0, mins_goal),
             pct_complete = ifelse(is.na(pct_complete), 0, pct_complete)) %>% 
      select(date, project_name, mins_complete, mins_goal, pct_complete, n_days_goal_entered)
    
    plot_title <- "Today"
    plot_caption <- str_c(round(totals$mins_complete, 0), " / ", totals$mins_goal, "m")
    
    
  } else if (time_pd == "week") {
    
    totals <- goals_df %>% 
      left_join(totals_df, by = c("week", "project_name")) %>% 
      filter(project_name == {{ project }}, 
             week == as.Date({{ date_ }})) %>% 
      mutate(mins_complete = secs / 60,
             pct_complete = mins_complete / mins_goal,
             # if missing, make 0
             mins_complete = ifelse(is.na(mins_complete), 0, mins_complete),
             mins_goal = ifelse(is.na(mins_goal), 0, mins_goal),
             pct_complete = ifelse(is.na(pct_complete), 0, pct_complete)) %>% 
      select(week, project_name, mins_complete, mins_goal, pct_complete, n_days_goal_entered)
    
    plot_title = "Week"
    plot_caption = str_c(round(totals$mins_complete / 60, 1), " / ", round(totals$mins_goal / 60, 1), "h")
    
    
  } else if (time_pd == "month") {
    
    totals <- goals_df %>% 
      left_join(totals_df, by = c("month", "project_name")) %>% 
      filter(project_name == {{ project }}, 
             month == as.Date({{ date_ }})) %>% 
      mutate(mins_complete = secs / 60,
             pct_complete = mins_complete / mins_goal,
             # if missing, make 0
             mins_complete = ifelse(is.na(mins_complete), 0, mins_complete),
             mins_goal = ifelse(is.na(mins_goal), 0, mins_goal),
             pct_complete = ifelse(is.na(pct_complete), 0, pct_complete)) %>% 
      select(month, project_name, mins_complete, mins_goal, pct_complete, n_days_goal_entered)
    
    plot_title = "Month"
    plot_caption = str_c(round(totals$mins_complete / 60, 0), " / ", round(totals$mins_goal / 60, 1), "h")    
    
    
  } else if (time_pd == "year") {
    
    totals <- 999
    
  }
  
  
  # if not enough goals entered, return blank plot
  if ( (time_pd == "day" & totals$n_days_goal_entered[1] == 0) | 
       (time_pd == "week" & totals$n_days_goal_entered[1] < 3) | 
       (time_pd == "month" & totals$n_days_goal_entered[1] < 10) ) {
    
    donut_plot <- ggplot() + ggplot2::theme_void()
    
    # else if goal was entered and plot should be displayed
  } else { 
    
    # if task not yet complete
    if (totals$mins_complete[1] < totals$mins_goal[1]) {
      
      plot_color <- "#fac32a" # orange-ish
      
      plot_df <- tibble::tribble(
        ~cat, ~ymin, ~ymax,
        "goal", 0, totals$pct_complete[1],
        "rest", totals$pct_complete[1], 1
      )
      
      # else if task complete
    } else {
      
      plot_color <- "green3"
      
      plot_df <- tibble::tribble(
        ~cat, ~ymin, ~ymax,
        "goal", 0, 1,
        "rest", 1, 1
      )
      
    }
    
    donut_plot <- plot_df %>% 
      # rectangle of fixed width where x% is colored (pct done) and the rest is grey
      ggplot(aes(ymin = ymin, ymax = ymax, xmin = 3, xmax = 4, fill = cat)) +
      geom_rect() +
      coord_polar(theta = "y", clip = "off") +
      # % value in middle of donut (95% done)
      annotate("text", x = 1.2, y = 1, size = 6, color = plot_color,
               label = str_c(signif(totals$pct_complete[1], 2) * 100, "%")) +
      xlim(1, 4) +
      ylim(0, 1) +
      scale_fill_manual(values = c(plot_color, "grey")) +
      guides(fill = "none") +
      labs(title = plot_title,
           # 25/90m label at bottom of chart
           caption = plot_caption) +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5, vjust = -2.25, size = 16),
            plot.caption = element_text(hjust = 0.5, vjust = 25, size = 8.2, color = "grey2"))
    
  }
  
  return(donut_plot)
  
} # END OF plot_donut() FCT

# test
#time_pd <- "day"
#totals_df <- daily_totals_df
#goals_df <- daily_goals_df
#project <- "pers-project"
#date_ <- "2023-05-12"

#plot_donut(time_pd, totals_df, goals_df, project, date_)


#plot_donut("day", daily_totals_df, daily_goals_df, "BlueLabs", "2023-05-10")
#plot_donut("week", weekly_totals_df, weekly_goals_df, "BlueLabs", "2023-05-01")
#plot_donut("week", weekly_totals_df, monthly_goals_df, "BlueLabs", "2023-05-08")

#plot_donut("day", daily_totals_df, daily_goals_df, "pers-project", "2023-05-11")
#plot_donut("week", weekly_totals_df, weekly_goals_df, "pers-project", "2023-05-01")
#plot_donut("week", weekly_totals_df, monthly_goals_df, "pers-project", "2023-05-08")

#plot_donut("day", daily_totals_df, daily_goals_df, "cooking/baking", "2023-05-11")
#plot_donut("week", weekly_totals_df, weekly_goals_df, "cooking/baking", "2023-05-01")
#plot_donut("week", weekly_totals_df, monthly_goals_df, "cooking/baking", "2023-05-08")



# task donut for the day will be 1-3 (sometimes 4 or 5) tasks that i want to work
# on that day

plot_task_donut <- function(totals_df, project, date_) {
  
  
  
}


make_donuts_box <- function(box_title, icon, box_id, plot_id) {
  box(
    title = span(box_title, icon), 
    id = box_id, 
    width = 11,
    
    # split into thirds -- one section for daily donut, one weekly, one monthly
    splitLayout(
      cellWidths = c("33.33%", "33.33%", "33.33%"),
      plotOutput(str_c("plot_", plot_id, "_donut_day"), height = "175px", width = "132px"),
      plotOutput(str_c("plot_", plot_id, "_donut_week"), height = "175px", width = "132px"),
      plotOutput(str_c("plot_", plot_id, "_donut_month"), height = "175px", width = "132px")
    )
  )
}

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
    
    # add outline to boxes (not currently working, it seems)
    tags$script(HTML("$('.box').eq(0).css('border', '5px solid #3DA0D1');")),
    
    tabItems(
      
      ## Donut Page ----------------------------------
      
      tabItem(tabName = "donut",
              
              # FIRST ROW
              
              splitLayout(
                # 3 boxes in the first row
                cellWidths = c("33.33%", "33.33%", "33.33%"),
                
                # BlueLabs donuts
                fluidRow(
                  make_donuts_box(box_title = "BlueLabs", 
                                  icon = icon("galactic-republic"),
                                  box_id = "bl_donuts",
                                  plot_id = "bl")
                ),
                
                # Organize/Build Skills donuts
                fluidRow(
                  make_donuts_box(box_title = "Organize/Build Skills", 
                                  icon = icon("seedling"),
                                  box_id = "organize_donuts",
                                  plot_id = "organize")
                ),
                
                # Read Books donuts
                fluidRow(
                  make_donuts_box(box_title = "Read Books",
                                  icon = icon("book"),
                                  box_id = "books_donut",
                                  plot_id = "books")
                )
              ),
              
              
              # SECOND ROW
              
              splitLayout(
                # 3 boxes in the first row
                cellWidths = c("33.33%", "33.33%", "33.33%"),
                
                # Chores/Responsibilities donuts
                fluidRow(
                  make_donuts_box(box_title = "Chores/Responsibilities",
                                  icon = icon("broom-wide"),
                                  box_id = "resp_donut",
                                  plot_id = "resp")
                ),
                
                
                # Learn Skill donuts
                fluidRow(
                  make_donuts_box(box_title = "Learn Skill",
                                  icon = icon("guitar"),
                                  box_id = "skill_donut",
                                  plot_id = "skill")
                ),
                
                
                # Cooking/Baking donuts
                fluidRow(
                  make_donuts_box(box_title = "Cooking/Baking",
                                  icon = icon("burger"),
                                  box_id = "cooking_donut",
                                  plot_id = "cooking")
                )
                
              ),
              
              
              # THIRD ROW
              
              splitLayout(
                # 3 boxes in the first row
                cellWidths = c("33.33%", "33.33%", "33.33%"),
                
                
                # Read/Watch Arts/Vids donuts
                fluidRow(
                  make_donuts_box(box_title = "Read/Watch Arts/Vids/News",
                                  icon = icon("newspaper"),
                                  box_id = "arts_donut",
                                  plot_id = "arts")
                ),
                
                
                # Review/Research donuts
                fluidRow(
                  make_donuts_box(box_title = "Review/Research",
                                  icon = icon("book"),
                                  box_id = "rr_donut",
                                  plot_id = "rr")
                ),
                
                
                # Journal/Plan donuts
                fluidRow(
                  make_donuts_box(box_title = "Journal/Plan",
                                  icon = icon("notebook"),
                                  box_id = "journal_donut",
                                  plot_id = "journal")
                )
                
              ),
              
              
              # FOURTH ROW OF BOXES (3 total)
              
              splitLayout(
                # 3 boxes in the first row
                cellWidths = c("33.33%", "33.33%", "33.33%"),
                
                
                # Stretch & Strength donuts
                fluidRow(
                  make_donuts_box(box_title = "Stretch & Strength",
                                  icon = icon("dumbbell"),
                                  box_id = "ss_donut",
                                  plot_id = "ss")
                ),
                
                # Exercise donuts
                fluidRow(
                  make_donuts_box(box_title = "Exercise",
                                  icon = icon("person-running"),
                                  box_id = "exercise_donut",
                                  plot_id = "exercise")
                ),
                
                # Personal Project donuts
                fluidRow(
                  make_donuts_box(box_title = "Exercise",
                                  icon = icon("checklist"),
                                  box_id = "proj_donut",
                                  plot_id = "proj")
                )
                
              )
              
              
      ), # end of donut page
      
      
      ## Today Page ----------------------------------
      
      # should only include tasks that have a goal for this day
      
      tabItem(tabName = "today",
              
              fluidRow(
                tableOutput("entries_table")
              )
      ),
      
      
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
    )
  )
)



# in the first tab, each box is a project-card with progress summaries (and
# some other info? list of most done/recent tasks in that project?) on front
# of card, and details found within clicks

# the second tab will have trends for all projects to see how they're 
# all working together

# create a function that'll build a new project box?

#boxProject()



# Server ----------------------------------------------------------------------

server <- function(input, output, session) {
  
  ## Read in toggl entries --------------------
  
  # read in updated entries df every minute (if folder is empty, i.e. .csv has 
  # just been deleted, then wait a few seconds)
  
  entries_raw <- reactive({
    invalidateLater(60000 * 3) # 60,000 ms = 1 minute
    
    if (length(list.files("/home/daniel/toggl-entries")) == 0) {
      Sys.sleep(3)
      read_csv("/home/daniel/toggl-entries/time_entries.csv")
    } else {
      read_csv("/home/daniel/toggl-entries/time_entries.csv")
    }
  })
  
  entries <- reactive({
    entries_raw() %>% 
      # subtracting 4 hours gets the actual time EST...
      # then i'm taking away another 4 hours so that if i do something at 2 or 3am 
      # (up to 4am here, using minus 4 hours), it'll count for the previous day
      mutate(start = start - lubridate::hours(8),
             stop = stop - lubridate::hours(8),
             date = lubridate::date(start))
  })
  
  daily_totals <- reactive({
    entries() %>% 
      group_by(date, project_name) %>% 
      summarise(secs = sum(duration)) %>% 
      ungroup()
  })
  
  weekly_totals <- reactive({
    entries() %>% 
      group_by(week = cut(date, "week"), project_name) %>% 
      summarise(secs = sum(duration)) %>% 
      ungroup() %>% 
      mutate(week = as.Date(week))
  })
  
  monthly_totals <- reactive({
    entries() %>% 
      group_by(month = cut(date, "month"), project_name) %>% 
      summarise(secs = sum(duration)) %>% 
      ungroup() %>% 
      mutate(month = as.Date(month))
  })
  
  
  
  ## Read in goals spreadsheet --------------------
  
  goals_raw <- reactive({
    invalidateLater(60000 * 15) # 60,000 ms = 1 minute
    read_csv("https://raw.githubusercontent.com/ddstats1/time-tracking/master/tracker/time_goals.csv")
  })
  
  daily_goals <- reactive({
    goals_raw() %>% 
      pivot_longer(cols = 2:ncol(.), 
                   names_to = "project_name", 
                   values_to = "mins_goal") %>% 
      mutate(date = lubridate::date(date),
             # janky but makes blank ggplot() return work nice...
             n_days_goal_entered = ifelse(mins_goal %in% c(NA, 0), 0, 1))
  })
  
  weekly_goals <- reactive({
    daily_goals() %>% 
      group_by(week = cut(date, "week"), project_name) %>% 
      summarise(mins_goal = sum(mins_goal, na.rm = TRUE),
                n_days_goal_entered = sum(n_days_goal_entered)) %>% 
      #ungroup() %>% 
      mutate(week = as.Date(week))
  })
  
  monthly_goals <- reactive({
    daily_goals() %>% 
      group_by(month = cut(date, "month"), project_name) %>% 
      summarise(mins_goal = sum(mins_goal, na.rm = TRUE),
                n_days_goal_entered = sum(n_days_goal_entered)) %>% 
      #ungroup() %>% 
      mutate(month = as.Date(month))
  })
  
  
  
  ## Donut Page plots ----------------------------
  
  
  ## BlueLabs donuts
  
  output$plot_bl_donut_day <- renderPlot({
    
    # if current time is between 12:00:01 am and 4:00:00 am, then want to display
    # plot from yesterday's date instead of Sys.Date()
    
    curr_time <- Sys.time() - hours(4)
    latenight_interval <- interval(ymd_hm(str_c(Sys.Date(), " 00:01")), 
                                   ymd_hm(str_c(Sys.Date(), " 04:00")))
    
    if (curr_time %within% latenight_interval) {
      date_adj <- Sys.Date() - days(1)
    } else {
      date_adj <- Sys.Date()
    }
    
    plot_donut(time_pd = "day", 
               totals_df = daily_totals(),
               goals_df = daily_goals(),
               project = "BlueLabs", 
               date_ = date_adj) 
  })
  
  # for week plots, go back to the Monday of this week
  output$plot_bl_donut_week <- renderPlot({
    plot_donut(time_pd = "week", 
               totals_df = weekly_totals(),
               goals_df = weekly_goals(),
               project = "BlueLabs", 
               date_ = lubridate::floor_date(Sys.Date(), "week", 1)) 
  })
  
  output$plot_bl_donut_month <- renderPlot({
    plot_donut(time_pd = "month", 
               totals_df = monthly_totals(),
               goals_df = monthly_goals(),
               project = "BlueLabs", 
               date_ = lubridate::floor_date(Sys.Date(), "month", 1))
  })
  
  
  ## Books donuts
  
  output$plot_books_donut_day <- renderPlot({
    
    curr_time <- Sys.time() - hours(4)
    latenight_interval <- interval(ymd_hm(str_c(Sys.Date(), " 00:01")), 
                                   ymd_hm(str_c(Sys.Date(), " 04:00")))
    
    if (curr_time %within% latenight_interval) {
      date_adj <- Sys.Date() - days(1)
    } else {
      date_adj <- Sys.Date()
    }
    
    plot_donut(time_pd = "day", 
               totals_df = daily_totals(),
               goals_df = daily_goals(),
               project = "read-books", 
               date_ = date_adj)
  })
  
  output$plot_books_donut_week <- renderPlot({
    plot_donut(time_pd = "week", 
               totals_df = weekly_totals(),
               goals_df = weekly_goals(),
               project = "read-books", 
               date_ = lubridate::floor_date(Sys.Date(), "week", 1)) 
  })
  
  output$plot_books_donut_month <- renderPlot({
    plot_donut(time_pd = "month", 
               totals_df = monthly_totals(),
               goals_df = monthly_goals(),
               project = "read-books", 
               date_ = lubridate::floor_date(Sys.Date(), "month", 1)) 
  })
  
  
  ## Organize / Build Skills donuts
  
  output$plot_organize_donut_day <- renderPlot({
    
    curr_time <- Sys.time() - hours(4)
    latenight_interval <- interval(ymd_hm(str_c(Sys.Date(), " 00:01")), 
                                   ymd_hm(str_c(Sys.Date(), " 04:00")))
    
    if (curr_time %within% latenight_interval) {
      date_adj <- Sys.Date() - days(1)
    } else {
      date_adj <- Sys.Date()
    }
    
    plot_donut(time_pd = "day", 
               totals_df = daily_totals(),
               goals_df = daily_goals(),
               project = "organize/build-skills", 
               date_ = date_adj) 
  })
  
  output$plot_organize_donut_week <- renderPlot({
    plot_donut(time_pd = "week", 
               totals_df = weekly_totals(),
               goals_df = weekly_goals(),
               project = "organize/build-skills", 
               date_ = lubridate::floor_date(Sys.Date(), "week", 1)) 
  })
  
  output$plot_organize_donut_month <- renderPlot({
    plot_donut(time_pd = "month", 
               totals_df = monthly_totals(),
               goals_df = monthly_goals(),
               project = "organize/build-skills", 
               date_ = lubridate::floor_date(Sys.Date(), "month", 1)) 
  })
  
  
  ## Pers Project donuts
  
  output$plot_proj_donut_day <- renderPlot({
    
    curr_time <- Sys.time() - hours(4)
    latenight_interval <- interval(ymd_hm(str_c(Sys.Date(), " 00:01")), 
                                   ymd_hm(str_c(Sys.Date(), " 04:00")))
    
    if (curr_time %within% latenight_interval) {
      date_adj <- Sys.Date() - days(1)
    } else {
      date_adj <- Sys.Date()
    }
    
    plot_donut(time_pd = "day", 
               totals_df = daily_totals(),
               goals_df = daily_goals(),
               project = "pers-project", 
               date_ = date_adj) 
  })
  
  output$plot_proj_donut_week <- renderPlot({
    plot_donut(time_pd = "week", 
               totals_df = weekly_totals(),
               goals_df = weekly_goals(),
               project = "pers-project", 
               date_ = lubridate::floor_date(Sys.Date(), "week", 1)) 
  })
  
  output$plot_proj_donut_month <- renderPlot({
    plot_donut(time_pd = "month", 
               totals_df = monthly_totals(),
               goals_df = monthly_goals(),
               project = "pers-project", 
               date_ = lubridate::floor_date(Sys.Date(), "month", 1)) 
  })
  
  
  ## Learn Skill donuts
  
  output$plot_skill_donut_day <- renderPlot({
    
    curr_time <- Sys.time() - hours(4)
    latenight_interval <- interval(ymd_hm(str_c(Sys.Date(), " 00:01")), 
                                   ymd_hm(str_c(Sys.Date(), " 04:00")))
    
    if (curr_time %within% latenight_interval) {
      date_adj <- Sys.Date() - days(1)
    } else {
      date_adj <- Sys.Date()
    }
    
    plot_donut(time_pd = "day", 
               totals_df = daily_totals(),
               goals_df = daily_goals(),
               project = "learn-skill", 
               date_ = date_adj) 
  })
  
  output$plot_skill_donut_week <- renderPlot({
    plot_donut(time_pd = "week", 
               totals_df = weekly_totals(),
               goals_df = weekly_goals(),
               project = "learn-skill", 
               date_ = lubridate::floor_date(Sys.Date(), "week", 1)) 
  })
  
  output$plot_skill_donut_month <- renderPlot({
    plot_donut(time_pd = "month", 
               totals_df = monthly_totals(),
               goals_df = monthly_goals(),
               project = "learn-skill", 
               date_ = lubridate::floor_date(Sys.Date(), "month", 1)) 
  })
  
  
  ## Cooking/Baking donuts
  
  output$plot_cooking_donut_day <- renderPlot({
    
    curr_time <- Sys.time() - hours(4)
    latenight_interval <- interval(ymd_hm(str_c(Sys.Date(), " 00:01")), 
                                   ymd_hm(str_c(Sys.Date(), " 04:00")))
    
    if (curr_time %within% latenight_interval) {
      date_adj <- Sys.Date() - days(1)
    } else {
      date_adj <- Sys.Date()
    }
    
    plot_donut(time_pd = "day", 
               totals_df = daily_totals(),
               goals_df = daily_goals(),
               project = "cooking/baking", 
               date_ = date_adj) 
  })
  
  output$plot_cooking_donut_week <- renderPlot({
    plot_donut(time_pd = "week", 
               totals_df = weekly_totals(),
               goals_df = weekly_goals(),
               project = "cooking/baking", 
               date_ = lubridate::floor_date(Sys.Date(), "week", 1)) 
  })
  
  output$plot_cooking_donut_month <- renderPlot({
    plot_donut(time_pd = "month", 
               totals_df = monthly_totals(),
               goals_df = monthly_goals(),
               project = "cooking/baking", 
               date_ = lubridate::floor_date(Sys.Date(), "month", 1)) 
  })
  
  
  ## Stretch & Strength donuts
  
  output$plot_ss_donut_day <- renderPlot({
    
    curr_time <- Sys.time() - hours(4)
    latenight_interval <- interval(ymd_hm(str_c(Sys.Date(), " 00:01")), 
                                   ymd_hm(str_c(Sys.Date(), " 04:00")))
    
    if (curr_time %within% latenight_interval) {
      date_adj <- Sys.Date() - days(1)
    } else {
      date_adj <- Sys.Date()
    }
    
    plot_donut(time_pd = "day", 
               totals_df = daily_totals(),
               goals_df = daily_goals(),
               project = "stretch & strength", 
               date_ = date_adj) 
  })
  
  output$plot_ss_donut_week <- renderPlot({
    plot_donut(time_pd = "week", 
               totals_df = weekly_totals(),
               goals_df = weekly_goals(),
               project = "stretch & strength", 
               date_ = lubridate::floor_date(Sys.Date(), "week", 1)) 
  })
  
  output$plot_ss_donut_month <- renderPlot({
    plot_donut(time_pd = "month", 
               totals_df = monthly_totals(),
               goals_df = monthly_goals(),
               project = "stretch & strength", 
               date_ = lubridate::floor_date(Sys.Date(), "month", 1)) 
  })
  
  
  ## Exercise donuts
  
  output$plot_exercise_donut_day <- renderPlot({
    
    curr_time <- Sys.time() - hours(4)
    latenight_interval <- interval(ymd_hm(str_c(Sys.Date(), " 00:01")), 
                                   ymd_hm(str_c(Sys.Date(), " 04:00")))
    
    if (curr_time %within% latenight_interval) {
      date_adj <- Sys.Date() - days(1)
    } else {
      date_adj <- Sys.Date()
    }
    
    plot_donut(time_pd = "day", 
               totals_df = daily_totals(),
               goals_df = daily_goals(),
               project = "exercise", 
               date_ = date_adj) 
  })
  
  output$plot_exercise_donut_week <- renderPlot({
    plot_donut(time_pd = "week", 
               totals_df = weekly_totals(),
               goals_df = weekly_goals(),
               project = "exercise", 
               date_ = lubridate::floor_date(Sys.Date(), "week", 1)) 
  })
  
  output$plot_exercise_donut_month <- renderPlot({
    plot_donut(time_pd = "month", 
               totals_df = monthly_totals(),
               goals_df = monthly_goals(),
               project = "exercise", 
               date_ = lubridate::floor_date(Sys.Date(), "month", 1)) 
  })
  
  
  ## Review/Research donuts
  
  output$plot_rr_donut_day <- renderPlot({
    
    curr_time <- Sys.time() - hours(4)
    latenight_interval <- interval(ymd_hm(str_c(Sys.Date(), " 00:01")), 
                                   ymd_hm(str_c(Sys.Date(), " 04:00")))
    
    if (curr_time %within% latenight_interval) {
      date_adj <- Sys.Date() - days(1)
    } else {
      date_adj <- Sys.Date()
    }
    
    plot_donut(time_pd = "day", 
               totals_df = daily_totals(),
               goals_df = daily_goals(),
               project = "review/research", 
               date_ = date_adj) 
  })
  
  output$plot_rr_donut_week <- renderPlot({
    plot_donut(time_pd = "week", 
               totals_df = weekly_totals(),
               goals_df = weekly_goals(),
               project = "review/research", 
               date_ = lubridate::floor_date(Sys.Date(), "week", 1)) 
  })
  
  output$plot_rr_donut_month <- renderPlot({
    plot_donut(time_pd = "month", 
               totals_df = monthly_totals(),
               goals_df = monthly_goals(),
               project = "review/research", 
               date_ = lubridate::floor_date(Sys.Date(), "month", 1)) 
  })
  
  
  ## Read/Watch Arts/Vids/News donuts
  
  output$plot_arts_donut_day <- renderPlot({
    
    curr_time <- Sys.time() - hours(4)
    latenight_interval <- interval(ymd_hm(str_c(Sys.Date(), " 00:01")), 
                                   ymd_hm(str_c(Sys.Date(), " 04:00")))
    
    if (curr_time %within% latenight_interval) {
      date_adj <- Sys.Date() - days(1)
    } else {
      date_adj <- Sys.Date()
    }
    
    plot_donut(time_pd = "day", 
               totals_df = daily_totals(),
               goals_df = daily_goals(),
               project = "read/watch-arts/vids/news", 
               date_ = date_adj) 
  })
  
  output$plot_arts_donut_week <- renderPlot({
    plot_donut(time_pd = "week", 
               totals_df = weekly_totals(),
               goals_df = weekly_goals(),
               project = "read/watch-arts/vids/news", 
               date_ = lubridate::floor_date(Sys.Date(), "week", 1)) 
  })
  
  output$plot_arts_donut_month <- renderPlot({
    plot_donut(time_pd = "month", 
               totals_df = monthly_totals(),
               goals_df = monthly_goals(),
               project = "read/watch-arts/vids/news", 
               date_ = lubridate::floor_date(Sys.Date(), "month", 1)) 
  })
  
  
  ## Responsibilities/Chores donuts
  
  output$plot_resp_donut_day <- renderPlot({
    
    curr_time <- Sys.time() - hours(4)
    latenight_interval <- interval(ymd_hm(str_c(Sys.Date(), " 00:01")), 
                                   ymd_hm(str_c(Sys.Date(), " 04:00")))
    
    if (curr_time %within% latenight_interval) {
      date_adj <- Sys.Date() - days(1)
    } else {
      date_adj <- Sys.Date()
    }
    
    plot_donut(time_pd = "day", 
               totals_df = daily_totals(),
               goals_df = daily_goals(),
               project = "responsibilities/chores", 
               date_ = date_adj) 
  })
  
  output$plot_resp_donut_week <- renderPlot({
    plot_donut(time_pd = "week", 
               totals_df = weekly_totals(),
               goals_df = weekly_goals(),
               project = "responsibilities/chores", 
               date_ = lubridate::floor_date(Sys.Date(), "week", 1)) 
  })
  
  output$plot_resp_donut_month <- renderPlot({
    plot_donut(time_pd = "month", 
               totals_df = monthly_totals(),
               goals_df = monthly_goals(),
               project = "responsibilities/chores", 
               date_ = lubridate::floor_date(Sys.Date(), "month", 1)) 
  })
  
  
  ## Journal/Plan donuts
  
  output$plot_journal_donut_day <- renderPlot({
    
    curr_time <- Sys.time() - hours(4)
    latenight_interval <- interval(ymd_hm(str_c(Sys.Date(), " 00:01")), 
                                   ymd_hm(str_c(Sys.Date(), " 04:00")))
    
    if (curr_time %within% latenight_interval) {
      date_adj <- Sys.Date() - days(1)
    } else {
      date_adj <- Sys.Date()
    }
    
    plot_donut(time_pd = "day", 
               totals_df = daily_totals(),
               goals_df = daily_goals(),
               project = "journal/plan", 
               date_ = date_adj) 
  })
  
  output$plot_journal_donut_week <- renderPlot({
    plot_donut(time_pd = "week", 
               totals_df = weekly_totals(),
               goals_df = weekly_goals(),
               project = "journal/plan", 
               date_ = lubridate::floor_date(Sys.Date(), "week", 1)) 
  })
  
  output$plot_journal_donut_month <- renderPlot({
    plot_donut(time_pd = "month", 
               totals_df = monthly_totals(),
               goals_df = monthly_goals(),
               project = "journal/plan", 
               date_ = lubridate::floor_date(Sys.Date(), "month", 1)) 
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


