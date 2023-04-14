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

## Read in time entries ----------------------------

#gs4_auth("dgdulaney1@gmail.com")

#entries_df <- read_sheet("1HAx_fVyI134wue7rroh_FaLh9DxlIQK8VQiNyD8m5cQ", sheet = "Sheet1") %>% 
#  mutate(date = lubridate::date(start))

entries_df <- read_csv("https://raw.githubusercontent.com/ddstats1/time-goals/main/time_entries_2023-04-14.csv") %>% 
  mutate(date = lubridate::date(start))

# get df's with daily/weekly/monthly/yearly totals by project
daily_totals <- entries_df %>% 
  group_by(date, project_name) %>% 
  summarise(secs = sum(duration)) %>% 
  ungroup()

weekly_totals <- 999

## Read in goals table ----------------------------

daily_goals_df_raw <- read_sheet("1JEtORWEfRUv_6sBLzQzXKrR5Bvc3hs3njQSm6y6Khtk", 
                                 sheet = "Daily Goals")

daily_goals_df <- daily_goals_df_raw %>% 
  pivot_longer(cols = BlueLabs:`time-with-family`, 
               names_to = "project_name", 
               values_to = "mins_goal") %>% 
  mutate(date = lubridate::date(date))

## Helper functions ----------------------------

# to turn values in a column into clean_names() output
clean_some_names <- function(dat, idx, ...) {
  names(dat)[idx] <- janitor::make_clean_names(names(dat)[idx], ...)
  dat
}

plot_daily_donut <- function(df, project, date_) {
  
  
  # WILL THIS GO INTO THE SERVER AT SOME POINT? SO THAT THIS GOALS DF
  # WILL CHANGE APPROPRIATELY WHEN I INPUT NEW GOALS
  
  day_totals <- daily_goals_df %>% 
    left_join(daily_totals, by = c("date", "project_name")) %>% 
    filter(project_name == {{ project }}, 
           date == as.Date({{ date_ }})) %>% 
    mutate(mins_complete = secs / 60,
           # if missing, then nothing tracked yet
           mins_complete = ifelse(is.na(mins_complete), 0, mins_complete),
           pct_complete = mins_complete / mins_goal) %>% 
    select(date, project_name, mins_complete, mins_goal, pct_complete)
    
  day_plot_df <- tibble::tribble(
    ~cat, ~ymin, ~ymax,
    "goal", 0, day_totals$pct_complete[1],
    "rest", day_totals$pct_complete[1], 1
    )
  
  day_plot_df %>% 
    # rectangle of fixed width where x% is colored (pct done) and the rest is grey
    # displayed with % in middle and __ / __ mins below, just like on Hours
    ggplot(aes(ymin = ymin, ymax = ymax, xmin = 3, xmax = 4, fill = cat)) +
    geom_rect() +
    coord_polar(theta = "y", clip = "off") +
    # % value in middle of donut (95% done)
    annotate("text", x = 1.2, y = 1, size = 5, color = "#fac32a",
             label = str_c(signif(day_totals$pct_complete[1], 2) * 100, "%")) +
    # minutes/hours left
    #annotate("text", x = 1, y = 1, size = 5, color = "#fac32a",
    #         label = str_c(day_totals$mins, "m//", day_totals$mins_goal, "m")) +
    
    xlim(1, 4) +
    ylim(0, 1) +
    scale_fill_manual(values = c("#fac32a", "grey")) +
    # remove legend
    guides(fill = "none") +
    labs(title = "Today",
         caption = str_c(day_totals$mins_complete, "/", day_totals$mins_goal, "m")) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, vjust = -2.25, size = 20),
          plot.caption = element_text(hjust = 0.5, vjust = 25, size = 10))
}

# test
plot_daily_donut(df = entries_df, project = "BlueLabs", date = "2023-04-14") # change to Sys.Date() after testing

plot_weekly_donut <- function() {
  
  weekly_totals_bl <- df %>% 
    filter(project_name == "BlueLabs") %>% 
    group_by(project_name, week = cut(start, "week")) %>% 
    summarise(x = sum(duration)) %>% 
    ungroup() %>% 
    mutate()
}

plot_monthly_donut <- function() {
  
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
    # can slice if necessary, as well
}

boxProject <- function(project) {
  
}




# UI ----------------------------------------------------------------------

ui <- dashboardPage(
  title = "Box API",
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Track", tabName = "track"),
      menuItem("Report", tabName = "report")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "track",
        fluidRow(
          box(
            title = "BlueLabs", 
            id = "bl_track", 
            width = 12,
            # to get the 3 plots and table to show side-by-side
            # will update to accommodate weekly & monthly donut plots
            splitLayout(
              cellWidths = c("50%", "50%"),
              # left half of BL card
              plotOutput("plot_bl", height = "200px", width = "200px"),
              # right half of BL card
              box(
                title = NULL,
                headerBorder = FALSE,
                id = "bl_tasks_box",
                width = 12,
                collapsible = FALSE,
                # to get date inputs side-by-side
                splitLayout(
                  cellWidths = c("50%", "50%"),
                  dateInput("bl_tasks_start", "Start Date", value = Sys.Date() - 7),
                  dateInput("bl_tasks_end", "End Date", value = Sys.Date())
                ),
                div(DT::dataTableOutput("top_tasks_bl"), style = "font-size: 70%; width: 70%")
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Read Books", 
            id = "books_track", 
            width = 12,
            column(width = 3, plotOutput("plot_books", height = "200px", width = "200px"))
            )
          )
      ),
      
      tabItem(tabName = "report",
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
    
    tags$head(tags$style('#foo .box-header{ display: none}'))  # target the box header of foo
    
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
  
  output$plot_bl <- renderPlot({
    plot_daily_donut(df = entries_df, project = "BlueLabs", date = Sys.Date()) # change to Sys.Date() after testing
  })
  
  output$plot_books <- renderPlot({
    plot_daily_donut(df = entries_df, project = "read-books", date = Sys.Date()) # change to Sys.Date() after testing
  })
  
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