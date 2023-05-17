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
library(calendR)

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

plot_donut <- function(time_pd = c("day", "week", "month", "year"), totals_df, goals_df, project, date_, is_today_page = FALSE) {
  
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
    
    # for donut page
    if (is_today_page == FALSE) {
      plot_title <- "Today"
      plot_caption <- str_c(round(totals$mins_complete, 0), " / ", totals$mins_goal, "m")
      # for today page
    } else {
      plot_title <- str_to_title(project)
      plot_caption <- str_c(round(totals$mins_complete, 0), " / ", totals$mins_goal, "m")
    }
    
    
    
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
            plot.caption = element_text(hjust = 0.5, vjust = 20, size = 8.2, color = "grey2"))
    
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






plot_calendar <- function(start_date, end_date, project, totals_df, goals_df) {
  
  vec_completed <- goals_df %>% 
    left_join(totals_df, by = c("date", "project_name")) %>% 
    filter(project_name == {{ project }},
           date >= start_date,
           date <= end_date) %>% 
    mutate(mins_complete = secs / 60,
           # if missing, make 0
           mins_complete = ifelse(is.na(mins_complete), 0, mins_complete),
           mins_goal = ifelse(is.na(mins_goal), 0, mins_goal),
           did_complete = case_when(date > date(Sys.time() - hours(4)) ~ NA_character_,
                                    mins_goal == 0 ~ NA_character_,
                                    mins_complete >= mins_goal ~ "1",
                                    # don't want today to be red if haven't completed
                                    date == date(Sys.time() - hours(4)) & (mins_complete < mins_goal) ~ NA_character_,
                                    mins_complete < mins_goal ~ "0")) %>% 
    pull(did_complete)
  
  # make today NA if it's currently "0" (so if complete, keep at "1")
  
  #if (!is.na(vec_completed[length(vec_completed)])) {
  #  
  #  vec_completed[length(vec_completed)] <- NA
  #}
  
  # IF only NA and 0's (i.e. haven't completed a day for this project, which
  # is cool, i want new projects from time to time), 
  # THEN change the very first NA to a "1"
  
  if (length(unique(vec_completed)) < 3) {
    
    loc_to_change <- which(is.na(vec_completed), NA_character_)[1]
    vec_completed[loc_to_change] <- "1"
    
  }
  
  # x / y days completed in latest month
  # end_month <- month(end_date)
  # end_year <- year(end_date)
  # 
  # end_month_totals <- daily_goals_df %>% 
  #   left_join(daily_totals_df, by = c("date", "project_name")) %>% 
  #   filter(project_name == {{ project }},
  #          month(date) == end_month,
  #          year(date) == end_year) %>% 
  #   mutate(mins_complete = secs / 60,
  #          # if missing, make 0
  #          mins_complete = ifelse(is.na(mins_complete), 0, mins_complete),
  #          mins_goal = ifelse(is.na(mins_goal), 0, mins_goal),
  #          did_complete = case_when(date > date(Sys.time() - hours(4)) ~ NA_character_,
  #                                   mins_goal == 0 ~ NA_character_,
  #                                   mins_complete >= mins_goal ~ "1",
  #                                   # don't want today to be red if haven't completed
  #                                   date == date(Sys.time() - hours(4)) & (mins_complete < mins_goal) ~ NA_character_,
  #                                   mins_complete < mins_goal ~ "0"))
  # 
  # days_met <- end_month_totals %>% filter(did_complete == "1") %>% nrow()
  # days_not_met <- end_month_totals %>% filter(did_complete == "0") %>% nrow()
  # 
  cal <- calendR(
    start_date = start_date,
    end_date = end_date,
    special.days = vec_completed,
    special.col = 2:3#,
    #title = str_c(month.name[end_month], ":  ", days_met, "/", days_met + days_not_met),
    #title.size = 12
  )
  
  return(cal)
  
}

#plot_calendar(start_date = "2023-05-01", end_date = "2023-05-15", project = "BlueLabs")



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


make_calendar_box <- function(project, id) {
  box(
    title = str_to_title(project),
    id = id,
    width = 11,
    
    dateRangeInput(str_c(id, "_cal_dates"), "", 
                   # default range: last month and this month
                   start = str_c(year(date(Sys.time() - hours(4))), "-", "0", month(date(Sys.time() - hours(4))) - 1, "-01"),
                   end = lubridate::ceiling_date(date(Sys.time() - hours(4)), "month") - lubridate::days(1)),
    
    plotOutput(str_c(id, "_cal_plot"))
  ) 
}



# top 5 tasks done for a specific project over the last week (and would be
# cool to be able to switch to last 2/3 weeks, last month, last 4 months, etc)
get_top_tasks <- function(project, start_date, end_date) {
  
  top_tasks <- entries_df %>% 
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
  
  return(top_tasks)
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
      menuItem("Calendars", tabName = "calendars"),
      menuItem("Tasks", tabName = "tasks"),
      menuItem("------------------", tabName = "--_1"),
      menuItem("Books", tabName = "books"),
      menuItem("Exercise", tabName = "exercise"),
      menuItem("------------------", tabName = "--_2"),
      menuItem("Reasons", tabName = "reasons")
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
      
      tabItem(
        tabName = "donut",
        
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
          
          
          # articles/essays/videos/news
          fluidRow(
            make_donuts_box(box_title = "articles/essays/videos/news",
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
            make_donuts_box(box_title = "Personal Projects",
                            icon = icon("checklist"),
                            box_id = "proj_donut",
                            plot_id = "proj")
          )
          
        )
        
        
      ), # end of donut page
      
      
      ## Today Page ----------------------------------
      
      tabItem(
        tabName = "today",
        
        # 
        # fluidRow(
        #   
        #   # two boxes in first row
        #   splitLayout(
        #     cellWidths = c("50%", "50%"),
        #     
        #     box(
        #       title = "", 
        #       id = "zzz_today", 
        #       width = 12,
        #       
        #       splitLayout(
        #         #cellWidths = c("10%", "10%", "10%", "10%", "10%", 
        #         #               "10%", "10%", "10%", "10%", "10%"),
        #         
        #         cellWidths = c("25%", "25%", "25%", "25%"),
        #         
        #         #cellWidths = c("33.33%", "33.33%"),
        #         
        #         plotOutput("plot_bl_today", height = "175px", width = "132px"),
        #         plotOutput("plot_organize_today", height = "175px", width = "132px"),
        #         plotOutput("plot_book_today", height = "175px", width = "132px"),
        #         plotOutput("plot_chores_today", height = "175px", width = "132px"),
        #         #plotOutput("plot_chores_today", height = "175px", width = "132px")
        #         #plotOutput("plot_chores_today", height = "175px", width = "132px"),
        #         #plotOutput("plot_chores_today", height = "175px", width = "132px"),
        #         #plotOutput("plot_chores_today", height = "175px", width = "132px"),
        #         #plotOutput("plot_chores_today", height = "175px", width = "132px"),
        #         #plotOutput("plot_chores_today", height = "175px", width = "132px")
        #       )
        #     ),
        #     
        #     box(
        #       title = "", 
        #       id = "zzz_today", 
        #       width = 12,
        #       
        #       splitLayout(
        #         #cellWidths = c("10%", "10%", "10%", "10%", "10%", 
        #         #               "10%", "10%", "10%", "10%", "10%"),
        #         
        #         cellWidths = c("25%", "25%", "25%", "25%"),
        #         
        #         #cellWidths = c("33.33%", "33.33%"),
        #         
        #         plotOutput("plot_bl_today", height = "175px", width = "132px"),
        #         plotOutput("plot_organize_today", height = "175px", width = "132px"),
        #         plotOutput("plot_book_today", height = "175px", width = "132px"),
        #         plotOutput("plot_chores_today", height = "175px", width = "132px"),
        #         #plotOutput("plot_chores_today", height = "175px", width = "132px")
        #         #plotOutput("plot_chores_today", height = "175px", width = "132px"),
        #         #plotOutput("plot_chores_today", height = "175px", width = "132px"),
        #         #plotOutput("plot_chores_today", height = "175px", width = "132px"),
        #         #plotOutput("plot_chores_today", height = "175px", width = "132px"),
        #         #plotOutput("plot_chores_today", height = "175px", width = "132px")
        #       )
        #     )
        #   )
        # )
        
      ),
      
      
      ## Calendars Page ----------------------------------
      
      tabItem(
        tabName = "calendars",
        
        # FIRST ROW
        
        splitLayout(
          cellWidths = c("33.33%", "33.33%", "33.33%"),
          
          fluidRow(make_calendar_box(project = "BlueLabs", id = "bl")),
          fluidRow(make_calendar_box(project = "organize/build-skills", id = "org")),
          fluidRow(make_calendar_box(project = "read-books", id = "books"))
        ),
        
        
        # SECOND ROW
        
        splitLayout(
          cellWidths = c("33.33%", "33.33%", "33.33%"),
          
          fluidRow(make_calendar_box(project = "chores/responsibilities", id = "chores")),
          fluidRow(make_calendar_box(project = "learn-skill", id = "skill")),
          fluidRow(make_calendar_box(project = "cooking/baking", id = "cooking"))
        ),
        
        
        # THIRD ROW
        
        splitLayout(
          cellWidths = c("33.33%", "33.33%", "33.33%"),
          
          fluidRow(make_calendar_box(project = "articles/essays/videos/news", id = "arts")),
          fluidRow(make_calendar_box(project = "review/research", id = "research")),
          fluidRow(make_calendar_box(project = "journal/plan", id = "journal"))
        ),
        
        
        # FOURTH ROW
        
        splitLayout(
          cellWidths = c("33.33%", "33.33%", "33.33%"),
          
          fluidRow(make_calendar_box(project = "stretch & strength", id = "ss")),
          fluidRow(make_calendar_box(project = "exercise", id = "exercise")),
          fluidRow(make_calendar_box(project = "pers-project", id = "proj"))
        )
      ),
      
      
      ## Tasks Page ----------------------------------
      
      tabItem(tabName = "tasks",
              
              
              
              
      ),
      
      ## Trends/Totals Page ----------------------------------
      
      tabItem(
        tabName = "trends",
        
        
      ),
      
      
      ## Trends/Totals Page ----------------------------------
      
      tabItem(
        tabName = "books",
        
        
      ),
      
      
      ## Trends/Totals Page ----------------------------------
      
      tabItem(
        tabName = "exercise",
        
        
      ),
      
      
      ## Trends/Totals Page ----------------------------------
      
      tabItem(
        tabName = "reasons",
        
        
      )
      
    ) # end of tab items
    
  ) # end of dashboard body
  
) # end of dashboard page



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
    # plot from yesterday's date instead of date(Sys.time() - hours(4))
    
    
    # THE ACTUAL CURRENT TIME, EST
    curr_time_est <- Sys.time() - hours(4) # adjusting b/c default is UTC timezone, 4 hrs ahead
    
    # between midnight and 4am
    latenight_interval <- interval(ymd_hm(str_c(date(curr_time_est), " 00:01")), 
                                   ymd_hm(str_c(date(curr_time_est), " 04:00")))
    
    if (curr_time_est %within% latenight_interval) {
      date_adj <- date(curr_time_est) - days(1)
    } else {
      date_adj <- date(curr_time_est)
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
               date_ = lubridate::floor_date(date(Sys.time() - hours(4)), "week", 1)) 
  })
  
  output$plot_bl_donut_month <- renderPlot({
    plot_donut(time_pd = "month", 
               totals_df = monthly_totals(),
               goals_df = monthly_goals(),
               project = "BlueLabs", 
               date_ = lubridate::floor_date(date(Sys.time() - hours(4)), "month", 1))
  })
  
  
  ## Books donuts
  
  output$plot_books_donut_day <- renderPlot({
    
    # if current time is between 12:00:01 am and 4:00:00 am, then want to display
    # plot from yesterday's date instead of date(Sys.time() - hours(4))
    
    
    # THE ACTUAL CURRENT TIME, EST
    curr_time_est <- Sys.time() - hours(4) # adjusting b/c default is UTC timezone, 4 hrs ahead
    
    # between midnight and 4am
    latenight_interval <- interval(ymd_hm(str_c(date(curr_time_est), " 00:01")), 
                                   ymd_hm(str_c(date(curr_time_est), " 04:00")))
    
    if (curr_time_est %within% latenight_interval) {
      date_adj <- date(curr_time_est) - days(1)
    } else {
      date_adj <- date(curr_time_est)
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
               date_ = lubridate::floor_date(date(Sys.time() - hours(4)), "week", 1)) 
  })
  
  output$plot_books_donut_month <- renderPlot({
    plot_donut(time_pd = "month", 
               totals_df = monthly_totals(),
               goals_df = monthly_goals(),
               project = "read-books", 
               date_ = lubridate::floor_date(date(Sys.time() - hours(4)), "month", 1)) 
  })
  
  
  ## Organize / Build Skills donuts
  
  output$plot_organize_donut_day <- renderPlot({
    
    # if current time is between 12:00:01 am and 4:00:00 am, then want to display
    # plot from yesterday's date instead of date(Sys.time() - hours(4))
    
    
    # THE ACTUAL CURRENT TIME, EST
    curr_time_est <- Sys.time() - hours(4) # adjusting b/c default is UTC timezone, 4 hrs ahead
    
    # between midnight and 4am
    latenight_interval <- interval(ymd_hm(str_c(date(curr_time_est), " 00:01")), 
                                   ymd_hm(str_c(date(curr_time_est), " 04:00")))
    
    if (curr_time_est %within% latenight_interval) {
      date_adj <- date(curr_time_est) - days(1)
    } else {
      date_adj <- date(curr_time_est)
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
               date_ = lubridate::floor_date(date(Sys.time() - hours(4)), "week", 1)) 
  })
  
  output$plot_organize_donut_month <- renderPlot({
    plot_donut(time_pd = "month", 
               totals_df = monthly_totals(),
               goals_df = monthly_goals(),
               project = "organize/build-skills", 
               date_ = lubridate::floor_date(date(Sys.time() - hours(4)), "month", 1)) 
  })
  
  
  ## Pers Project donuts
  
  output$plot_proj_donut_day <- renderPlot({
    
    # if current time is between 12:00:01 am and 4:00:00 am, then want to display
    # plot from yesterday's date instead of date(Sys.time() - hours(4))
    
    
    # THE ACTUAL CURRENT TIME, EST
    curr_time_est <- Sys.time() - hours(4) # adjusting b/c default is UTC timezone, 4 hrs ahead
    
    # between midnight and 4am
    latenight_interval <- interval(ymd_hm(str_c(date(curr_time_est), " 00:01")), 
                                   ymd_hm(str_c(date(curr_time_est), " 04:00")))
    
    if (curr_time_est %within% latenight_interval) {
      date_adj <- date(curr_time_est) - days(1)
    } else {
      date_adj <- date(curr_time_est)
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
               date_ = lubridate::floor_date(date(Sys.time() - hours(4)), "week", 1)) 
  })
  
  output$plot_proj_donut_month <- renderPlot({
    plot_donut(time_pd = "month", 
               totals_df = monthly_totals(),
               goals_df = monthly_goals(),
               project = "pers-project", 
               date_ = lubridate::floor_date(date(Sys.time() - hours(4)), "month", 1)) 
  })
  
  
  ## Learn Skill donuts
  
  output$plot_skill_donut_day <- renderPlot({
    
    # if current time is between 12:00:01 am and 4:00:00 am, then want to display
    # plot from yesterday's date instead of date(Sys.time() - hours(4))
    
    
    # THE ACTUAL CURRENT TIME, EST
    curr_time_est <- Sys.time() - hours(4) # adjusting b/c default is UTC timezone, 4 hrs ahead
    
    # between midnight and 4am
    latenight_interval <- interval(ymd_hm(str_c(date(curr_time_est), " 00:01")), 
                                   ymd_hm(str_c(date(curr_time_est), " 04:00")))
    
    if (curr_time_est %within% latenight_interval) {
      date_adj <- date(curr_time_est) - days(1)
    } else {
      date_adj <- date(curr_time_est)
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
               date_ = lubridate::floor_date(date(Sys.time() - hours(4)), "week", 1)) 
  })
  
  output$plot_skill_donut_month <- renderPlot({
    plot_donut(time_pd = "month", 
               totals_df = monthly_totals(),
               goals_df = monthly_goals(),
               project = "learn-skill", 
               date_ = lubridate::floor_date(date(Sys.time() - hours(4)), "month", 1)) 
  })
  
  
  ## Cooking/Baking donuts
  
  output$plot_cooking_donut_day <- renderPlot({
    
    # if current time is between 12:00:01 am and 4:00:00 am, then want to display
    # plot from yesterday's date instead of date(Sys.time() - hours(4))
    
    
    # THE ACTUAL CURRENT TIME, EST
    curr_time_est <- Sys.time() - hours(4) # adjusting b/c default is UTC timezone, 4 hrs ahead
    
    # between midnight and 4am
    latenight_interval <- interval(ymd_hm(str_c(date(curr_time_est), " 00:01")), 
                                   ymd_hm(str_c(date(curr_time_est), " 04:00")))
    
    if (curr_time_est %within% latenight_interval) {
      date_adj <- date(curr_time_est) - days(1)
    } else {
      date_adj <- date(curr_time_est)
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
               date_ = lubridate::floor_date(date(Sys.time() - hours(4)), "week", 1)) 
  })
  
  output$plot_cooking_donut_month <- renderPlot({
    plot_donut(time_pd = "month", 
               totals_df = monthly_totals(),
               goals_df = monthly_goals(),
               project = "cooking/baking", 
               date_ = lubridate::floor_date(date(Sys.time() - hours(4)), "month", 1)) 
  })
  
  
  ## Stretch & Strength donuts
  
  output$plot_ss_donut_day <- renderPlot({
    
    # if current time is between 12:00:01 am and 4:00:00 am, then want to display
    # plot from yesterday's date instead of date(Sys.time() - hours(4))
    
    
    # THE ACTUAL CURRENT TIME, EST
    curr_time_est <- Sys.time() - hours(4) # adjusting b/c default is UTC timezone, 4 hrs ahead
    
    # between midnight and 4am
    latenight_interval <- interval(ymd_hm(str_c(date(curr_time_est), " 00:01")), 
                                   ymd_hm(str_c(date(curr_time_est), " 04:00")))
    
    if (curr_time_est %within% latenight_interval) {
      date_adj <- date(curr_time_est) - days(1)
    } else {
      date_adj <- date(curr_time_est)
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
               date_ = lubridate::floor_date(date(Sys.time() - hours(4)), "week", 1)) 
  })
  
  output$plot_ss_donut_month <- renderPlot({
    plot_donut(time_pd = "month", 
               totals_df = monthly_totals(),
               goals_df = monthly_goals(),
               project = "stretch & strength", 
               date_ = lubridate::floor_date(date(Sys.time() - hours(4)), "month", 1)) 
  })
  
  
  ## Exercise donuts
  
  output$plot_exercise_donut_day <- renderPlot({
    
    # if current time is between 12:00:01 am and 4:00:00 am, then want to display
    # plot from yesterday's date instead of date(Sys.time() - hours(4))
    
    
    # THE ACTUAL CURRENT TIME, EST
    curr_time_est <- Sys.time() - hours(4) # adjusting b/c default is UTC timezone, 4 hrs ahead
    
    # between midnight and 4am
    latenight_interval <- interval(ymd_hm(str_c(date(curr_time_est), " 00:01")), 
                                   ymd_hm(str_c(date(curr_time_est), " 04:00")))
    
    if (curr_time_est %within% latenight_interval) {
      date_adj <- date(curr_time_est) - days(1)
    } else {
      date_adj <- date(curr_time_est)
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
               date_ = lubridate::floor_date(date(Sys.time() - hours(4)), "week", 1)) 
  })
  
  output$plot_exercise_donut_month <- renderPlot({
    plot_donut(time_pd = "month", 
               totals_df = monthly_totals(),
               goals_df = monthly_goals(),
               project = "exercise", 
               date_ = lubridate::floor_date(date(Sys.time() - hours(4)), "month", 1)) 
  })
  
  
  ## Review/Research donuts
  
  output$plot_rr_donut_day <- renderPlot({
    
    # if current time is between 12:00:01 am and 4:00:00 am, then want to display
    # plot from yesterday's date instead of date(Sys.time() - hours(4))
    
    
    # THE ACTUAL CURRENT TIME, EST
    curr_time_est <- Sys.time() - hours(4) # adjusting b/c default is UTC timezone, 4 hrs ahead
    
    # between midnight and 4am
    latenight_interval <- interval(ymd_hm(str_c(date(curr_time_est), " 00:01")), 
                                   ymd_hm(str_c(date(curr_time_est), " 04:00")))
    
    if (curr_time_est %within% latenight_interval) {
      date_adj <- date(curr_time_est) - days(1)
    } else {
      date_adj <- date(curr_time_est)
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
               date_ = lubridate::floor_date(date(Sys.time() - hours(4)), "week", 1)) 
  })
  
  output$plot_rr_donut_month <- renderPlot({
    plot_donut(time_pd = "month", 
               totals_df = monthly_totals(),
               goals_df = monthly_goals(),
               project = "review/research", 
               date_ = lubridate::floor_date(date(Sys.time() - hours(4)), "month", 1)) 
  })
  
  
  ## articles/essays/videos/news donuts
  
  output$plot_arts_donut_day <- renderPlot({
    
    # if current time is between 12:00:01 am and 4:00:00 am, then want to display
    # plot from yesterday's date instead of date(Sys.time() - hours(4))
    
    
    # THE ACTUAL CURRENT TIME, EST
    curr_time_est <- Sys.time() - hours(4) # adjusting b/c default is UTC timezone, 4 hrs ahead
    
    # between midnight and 4am
    latenight_interval <- interval(ymd_hm(str_c(date(curr_time_est), " 00:01")), 
                                   ymd_hm(str_c(date(curr_time_est), " 04:00")))
    
    if (curr_time_est %within% latenight_interval) {
      date_adj <- date(curr_time_est) - days(1)
    } else {
      date_adj <- date(curr_time_est)
    }
    
    plot_donut(time_pd = "day", 
               totals_df = daily_totals(),
               goals_df = daily_goals(),
               project = "articles/essays/videos/news", 
               date_ = date_adj) 
  })
  
  output$plot_arts_donut_week <- renderPlot({
    plot_donut(time_pd = "week", 
               totals_df = weekly_totals(),
               goals_df = weekly_goals(),
               project = "articles/essays/videos/news", 
               date_ = lubridate::floor_date(date(Sys.time() - hours(4)), "week", 1)) 
  })
  
  output$plot_arts_donut_month <- renderPlot({
    plot_donut(time_pd = "month", 
               totals_df = monthly_totals(),
               goals_df = monthly_goals(),
               project = "articles/essays/videos/news", 
               date_ = lubridate::floor_date(date(Sys.time() - hours(4)), "month", 1)) 
  })
  
  
  ## Responsibilities/Chores donuts
  
  output$plot_resp_donut_day <- renderPlot({
    
    # if current time is between 12:00:01 am and 4:00:00 am, then want to display
    # plot from yesterday's date instead of date(Sys.time() - hours(4))
    
    
    # THE ACTUAL CURRENT TIME, EST
    curr_time_est <- Sys.time() - hours(4) # adjusting b/c default is UTC timezone, 4 hrs ahead
    
    # between midnight and 4am
    latenight_interval <- interval(ymd_hm(str_c(date(curr_time_est), " 00:01")), 
                                   ymd_hm(str_c(date(curr_time_est), " 04:00")))
    
    if (curr_time_est %within% latenight_interval) {
      date_adj <- date(curr_time_est) - days(1)
    } else {
      date_adj <- date(curr_time_est)
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
               date_ = lubridate::floor_date(date(Sys.time() - hours(4)), "week", 1)) 
  })
  
  output$plot_resp_donut_month <- renderPlot({
    plot_donut(time_pd = "month", 
               totals_df = monthly_totals(),
               goals_df = monthly_goals(),
               project = "responsibilities/chores", 
               date_ = lubridate::floor_date(date(Sys.time() - hours(4)), "month", 1)) 
  })
  
  
  ## Journal/Plan donuts
  
  output$plot_journal_donut_day <- renderPlot({
    
    # if current time is between 12:00:01 am and 4:00:00 am, then want to display
    # plot from yesterday's date instead of date(Sys.time() - hours(4))
    
    
    # THE ACTUAL CURRENT TIME, EST
    curr_time_est <- Sys.time() - hours(4) # adjusting b/c default is UTC timezone, 4 hrs ahead
    
    # between midnight and 4am
    latenight_interval <- interval(ymd_hm(str_c(date(curr_time_est), " 00:01")), 
                                   ymd_hm(str_c(date(curr_time_est), " 04:00")))
    
    if (curr_time_est %within% latenight_interval) {
      date_adj <- date(curr_time_est) - days(1)
    } else { 
      date_adj <- date(curr_time_est)
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
               date_ = lubridate::floor_date(date(Sys.time() - hours(4)), "week", 1)) 
  })
  
  output$plot_journal_donut_month <- renderPlot({
    plot_donut(time_pd = "month", 
               totals_df = monthly_totals(),
               goals_df = monthly_goals(),
               project = "journal/plan", 
               date_ = lubridate::floor_date(date(Sys.time() - hours(4)), "month", 1)) 
  })
  
  
  ## Today Page ----------------------------
  
  # output$plot_bl_today <- renderPlot({
  #   
  #   # BlueLabs
  #   if (curr_time_est %within% latenight_interval) {
  #     date_adj <- date(Sys.time() - hours(4)) - days(1)
  #   } else {
  #     date_adj <- date(Sys.time() - hours(4))
  #   }
  #   
  #   plot_donut(time_pd = "day", 
  #              totals_df = daily_totals(),
  #              goals_df = daily_goals(),
  #              project = "BlueLabs", 
  #              date_ = date_adj,
  #              is_today_page = TRUE) 
  # })
  # 
  # 
  # # Organize
  # output$plot_organize_today <- renderPlot({
  #   
  #   if (curr_time_est %within% latenight_interval) {
  #     date_adj <- date(Sys.time() - hours(4)) - days(1)
  #   } else {
  #     date_adj <- date(Sys.time() - hours(4))
  #   }
  #   
  #   plot_donut(time_pd = "day", 
  #              totals_df = daily_totals(),
  #              goals_df = daily_goals(),
  #              project = "organize/build-skills", 
  #              date_ = date_adj,
  #              is_today_page = TRUE) 
  # })
  # 
  # output$plot_book_today <- renderPlot({
  #   
  #   if (curr_time_est %within% latenight_interval) {
  #     date_adj <- date(Sys.time() - hours(4)) - days(1)
  #   } else {
  #     date_adj <- date(Sys.time() - hours(4))
  #   }
  #   
  #   plot_donut(time_pd = "day", 
  #              totals_df = daily_totals(),
  #              goals_df = daily_goals(),
  #              project = "read-books", 
  #              date_ = date_adj,
  #              is_today_page = TRUE) 
  # })
  # 
  # output$plot_chores_today <- renderPlot({
  #   
  #   if (curr_time_est %within% latenight_interval) {
  #     date_adj <- date(Sys.time() - hours(4)) - days(1)
  #   } else {
  #     date_adj <- date(Sys.time() - hours(4))
  #   }
  #   
  #   plot_donut(time_pd = "day", 
  #              totals_df = daily_totals(),
  #              goals_df = daily_goals(),
  #              project = "responsibilities/chores", 
  #              date_ = date_adj,
  #              is_today_page = TRUE) 
  # })
  
  
  ## Calendars Page ----------------------------
  
  output$bl_cal_plot <- renderPlot({
    plot_calendar(start_date = input$bl_cal_dates[1],
                  end_date = input$bl_cal_dates[2],
                  project = "BlueLabs",
                  totals_df = daily_totals(),
                  goals_df = daily_goals())
  })
  
  output$org_cal_plot <- renderPlot({
    plot_calendar(start_date = input$org_cal_dates[1],
                  end_date = input$org_cal_dates[2],
                  project = "organize/build-skills",
                  totals_df = daily_totals(),
                  goals_df = daily_goals())
  })
  
  output$books_cal_plot <- renderPlot({
    plot_calendar(start_date = input$books_cal_dates[1],
                  end_date = input$books_cal_dates[2],
                  project = "read-books",
                  totals_df = daily_totals(),
                  goals_df = daily_goals())
  })
  
  output$chores_cal_plot <- renderPlot({
    plot_calendar(start_date = input$chores_cal_dates[1],
                  end_date = input$chores_cal_dates[2],
                  project = "responsibilities/chores",
                  totals_df = daily_totals(),
                  goals_df = daily_goals())
  })
  
  output$skill_cal_plot <- renderPlot({
    plot_calendar(start_date = input$skill_cal_dates[1],
                  end_date = input$skill_cal_dates[2],
                  project = "learn-skill",
                  totals_df = daily_totals(),
                  goals_df = daily_goals())
  })
  
  output$cooking_cal_plot <- renderPlot({
    plot_calendar(start_date = input$cooking_cal_dates[1],
                  end_date = input$cooking_cal_dates[2],
                  project = "cooking/baking",
                  totals_df = daily_totals(),
                  goals_df = daily_goals())
  })
  
  output$arts_cal_plot <- renderPlot({
    plot_calendar(start_date = input$arts_cal_dates[1],
                  end_date = input$arts_cal_dates[2],
                  project = "articles/essays/videos/news",
                  totals_df = daily_totals(),
                  goals_df = daily_goals())
  })
  
  output$research_cal_plot <- renderPlot({
    plot_calendar(start_date = input$research_cal_dates[1],
                  end_date = input$research_cal_dates[2],
                  project = "review/research",
                  totals_df = daily_totals(),
                  goals_df = daily_goals())
  })
  
  output$journal_cal_plot <- renderPlot({
    plot_calendar(start_date = input$journal_cal_dates[1],
                  end_date = input$journal_cal_dates[2],
                  project = "journal/plan",
                  totals_df = daily_totals(),
                  goals_df = daily_goals())
  })
  
  output$ss_cal_plot <- renderPlot({
    plot_calendar(start_date = input$ss_cal_dates[1],
                  end_date = input$ss_cal_dates[2],
                  project = "stretch & strength",
                  totals_df = daily_totals(),
                  goals_df = daily_goals())
  })
  
  output$exercise_cal_plot <- renderPlot({
    plot_calendar(start_date = input$exercise_cal_dates[1],
                  end_date = input$exercise_cal_dates[2],
                  project = "exercise",
                  totals_df = daily_totals(),
                  goals_df = daily_goals())
  })
  
  output$proj_cal_plot <- renderPlot({
    plot_calendar(start_date = input$proj_cal_dates[1],
                  end_date = input$proj_cal_dates[2],
                  project = "pers-project",
                  totals_df = daily_totals(),
                  goals_df = daily_goals())
  })
  
  
  
  ## Tasks Page ----------------------------
  
  
  ## Trends Page ----------------------------
  
  
}

# so will deploy properly
#keyring::keyring_unlock("system", password = "2022")

# run app locally
shinyApp(ui, server)

