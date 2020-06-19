# Shiny app for forcasting timing & volume of study visits.
# Author: Michelle VanTieghem
# Date: June 15, 2020

# set up environment and data ------------------------------------------------

library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(lubridate) 
set.seed(333) # for random sampling
# custom functions
source("scripts/custom_functions.R")

# open, and clean your data automatically by running these scripts:
source_rmd("scripts/data_setup_script.Rmd")


# set months of enrollment choices 
month_list <- levels(enrolled_clean$enrollment_month)
enrichment_list <- levels(enrolled_clean$enrichment_variable)
visit_list <- c('newborn', 'six_month', 'twelve_month', 'all_visits')

# USER INTERFACE -----------------------------------------------------

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # App title ----
  titlePanel("COPE Study Visit Forecasting"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for the number of bins ----
      
      checkboxGroupInput(inputId='enrollment_choice', 
                         label = "Select Subjects based on Enrollment Date",
                         choices = month_list, 
                         selected = month_list),
      checkboxGroupInput(inputId = 'status_choice',
                         label = "Select Subjects based on Status",
                         choices = c("Pregnant", "New Mom"),
                         selected = c("Pregnant", "New Mom")),
      checkboxGroupInput(inputId = 'enrichment_choice', 
                         label = "Select Subjects based on enrichment variable",
                        choices = enrichment_list, 
                         selected = enrichment_list),
      sliderInput(inputId = "N_participant_choice",
                  label = "Ratio of enrolled subjects to be tested:",
                  min = 0, max = 1, value = 0.5),
      checkboxGroupInput(inputId = 'visit_choice', 
                         label = 'Select Visit Type', 
                         choices = visit_list,
                         selected = visit_list),
      dateInput(inputId ='start_date', 
                label = 'Study Start Date',
                value = '2020-08-01', format = 'mm/dd/yyyy'),
      sliderInput(inputId = 'num_weeks_choice', 
                  label = 'Study Duration (in weeks)',
                  min = 3, max = 200, 
                  value =  80)
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # To make panel with tabs
      tabsetPanel(type = "tabs",
                  tabPanel("Summary", tableOutput(outputId = "summary_table")),
                  tabPanel("Figures", plotOutput(outputId = "plot_visits")), 
                  tabPanel("Tables", tableOutput(outputId = "visit_table")),
                  tabPanel("About", 
                           p(""),
                           h5('This tool was developed to plan study operations for longitudinal developmental studies.'), 
                           p('The schedule for infant testing visits is based on the gestational and/or infant age at enrollment, which is provided in the input data.
                           The schedule automatically updates based on several variables: 
                             selected participant characteristics, study start date, study duration, and target N. 
                             Select inputs on the sidebar to see how study visits & volume changes.'),
                           p (""), 
                           p("----------------------------------------------------------"), 
                           p("Author: Michelle VanTieghem"), 
                           p("For code and instructions to add your own data, see the github: LINK"))
                  
      )
    )
  )
)


# SERVER FUNCTION ---------------------------------------------------------

server_function <- function(input, output) {
 # input$N_participant_choice <- 0.5
  #input$enrollment_choice <- "April"
  #input$status_choice <- "Pregnant"
  #input$start_date <- ymd("2020-08-01")
  #input$visit_choice <- "newborn"
  #input$num_weeks_choice <- "20"
  
  ## FUNCTION FOR INTERACTIVE PLOT --------------------
  output$plot_visits <- renderPlot({
    
  # CLEAN DATA BASED ON INPUTS 
  # filter the dataset according to Number of participants chosen!
  enrolled_subset <- enrolled_clean %>%
    mutate(newborn_visit =child_birth_date,
           six_mo_visit =  child_birth_date + months(6),
           twelve_mo_visit =  child_birth_date + years(1)) %>% 
    # filter based on enrollment dates chosen and pregnant vs. new mom.
    filter(enrollment_month %in% input$enrollment_choice & 
             status %in% input$status_choice & 
             enrichment_variable %in% input$enrichment_choice)
  
  # calculate how many subjects you want to include in visits, based on proportion of enrolled
  N_to_model <- round(nrow(enrolled_subset) * input$N_participant_choice)
  enrolled_subset2 <- enrolled_subset %>%
    # get random sample from enrolled sample
    sample_n(N_to_model)
  
  # set up an empty schedule dataframe, based on duration chosen
  schedule_by_week <- data.frame(Week_Number = 1:200) %>%
    # calculate date intervals for each week 
    mutate(week_start = ymd(input$start_date) + weeks(1:200),
           week_end = ymd(input$start_date) + weeks(1:200) + days (7),
           week_interval = interval(week_start, week_end),
           # set variables to fill in 
           newborn = NA, 
           six_month = NA, 
           twelve_month = NA)
  
  # calculate weekly visit rate
  # for each week, count # of visits per session and save in schedule
  for (week in 1:nrow(schedule_by_week)){
    get_interval = schedule_by_week$week_interval[week]
    
    # INFANT VISITS with partial sample
    schedule_by_week$newborn[week] = sum(enrolled_subset2$newborn_visit[!is.na(enrolled_subset2$newborn_visit)] %within% get_interval)
    schedule_by_week$six_month[week] = sum(enrolled_subset2$six_mo_visit[!is.na(enrolled_subset2$six_mo_visit)] %within% get_interval)
    schedule_by_week$twelve_month[week] = sum(enrolled_subset2$twelve_mo_visit[!is.na(enrolled_subset2$twelve_mo_visit)] %within% get_interval)
  }
  # calculate total visits per week
  schedule_by_week <- schedule_by_week %>%
    mutate(all_visits = newborn + six_month + twelve_month)
  
  
  # make a long version of data for plotting and tables
  schedule_long_form <- schedule_by_week %>%
    gather(key = "Visit_Type", value = "Number_of_Visits",
           -week_start, -week_end, -week_interval,-Week_Number) 

   # plot
    plot_visits <- schedule_long_form %>%
      filter(Visit_Type %in% input$visit_choice & 
               Week_Number <= input$num_weeks_choice) %>%
      ggplot( aes(x = Week_Number, y = Number_of_Visits)) + 
      facet_wrap(~Visit_Type, nrow = 4) + 
      geom_line() + theme_bw() 
    
    # Display the plot with all of the choices incorporated
    plot_visits
    
  })
  # FUNCTION FOR INTERACTIVE TABLE ------------------------------
  output$visit_table <- renderTable({
    
    # CLEAN DATA BASED ON INPUTS 
    # filter the dataset according to Number of participants chosen!
    enrolled_subset <- enrolled_clean %>%
      mutate(newborn_visit =child_birth_date,
             six_mo_visit =  child_birth_date + months(6),
             twelve_mo_visit =  child_birth_date + years(1)) %>% 
      # filter based on enrollment dates chosen and pregnant vs. new mom.
      filter(enrollment_month %in% input$enrollment_choice & 
               status %in% input$status_choice & 
               enrichment_variable %in% input$enrichment_choice)
    
    # calculate how many subjects you want to include in visits, based on proportion of enrolled
    N_to_model <- round(nrow(enrolled_subset) * input$N_participant_choice)
    enrolled_subset2 <- enrolled_subset %>%
      # get random sample from enrolled sample
      sample_n(N_to_model)
    
    # set up an empty schedule dataframe, based on duration chosen
    schedule_by_week <- data.frame(Week_Number = 1:200) %>%
      # calculate date intervals for each week 
      mutate(week_start = ymd(input$start_date) + weeks(1:200),
             week_end = ymd(input$start_date) + weeks(1:200) + days (7),
             week_interval = interval(week_start, week_end),
             # set variables to fill in 
             newborn = NA, 
             six_month = NA, 
             twelve_month = NA)
    
    # calculate weekly visit rate
    # for each week, count # of visits per session and save in schedule
    for (week in 1:nrow(schedule_by_week)){
      get_interval = schedule_by_week$week_interval[week]
      
      # INFANT VISITS with partial sample
      schedule_by_week$newborn[week] = sum(enrolled_subset2$newborn_visit[!is.na(enrolled_subset2$newborn_visit)] %within% get_interval)
      schedule_by_week$six_month[week] = sum(enrolled_subset2$six_mo_visit[!is.na(enrolled_subset2$six_mo_visit)] %within% get_interval)
      schedule_by_week$twelve_month[week] = sum(enrolled_subset2$twelve_mo_visit[!is.na(enrolled_subset2$twelve_mo_visit)] %within% get_interval)
    }
    # calculate total visits per week
    schedule_by_week <- schedule_by_week %>%
      mutate(all_visits = newborn + six_month + twelve_month)
    
    
    # make a long version of data for plotting and tables
    schedule_long_form <- schedule_by_week %>%
      gather(key = "Visit_Type", value = "Number_of_Visits",
             -week_start, -week_end, -week_interval,-Week_Number) 
    
    # table
  visit_table <- schedule_long_form %>%
      filter(Visit_Type %in% input$visit_choice & 
              Week_Number <= input$num_weeks_choice) %>%
      mutate(week_start = as.character(week_start), 
             week_end = as.character(week_end))
  visit_table
  })

# FUNCTION FOR INTERACTIVE SUMMARY TABLE ------------------------------
output$summary_table <- renderTable({
  
  # CLEAN DATA BASED ON INPUTS 
  # filter the dataset according to Number of participants chosen!
  enrolled_subset <- enrolled_clean %>%
    mutate(newborn_visit =child_birth_date,
           six_mo_visit =  child_birth_date + months(6),
           twelve_mo_visit =  child_birth_date + years(1)) %>% 
    # filter based on enrollment dates chosen and pregnant vs. new mom.
    filter(enrollment_month %in% input$enrollment_choice & 
             status %in% input$status_choice & 
             enrichment_variable %in% input$enrichment_choice)
  
  # calculate how many subjects you want to include in visits, based on proportion of enrolled
  N_to_model <- round(nrow(enrolled_subset) * input$N_participant_choice)
  enrolled_subset2 <- enrolled_subset %>%
    # get random sample from enrolled sample
    sample_n(N_to_model)
  
  # set up an empty schedule dataframe, based on duration chosen
  schedule_by_week <- data.frame(Week_Number = 1:200) %>%
    # calculate date intervals for each week 
    mutate(week_start = ymd(input$start_date) + weeks(1:200),
           week_end = ymd(input$start_date) + weeks(1:200) + days (7),
           week_interval = interval(week_start, week_end),
           # set variables to fill in 
           newborn = NA, 
           six_month = NA, 
           twelve_month = NA)
  
  # calculate weekly visit rate
  # for each week, count # of visits per session and save in schedule
  for (week in 1:nrow(schedule_by_week)){
    get_interval = schedule_by_week$week_interval[week]
    
    # INFANT VISITS with partial sample
    schedule_by_week$newborn[week] = sum(enrolled_subset2$newborn_visit[!is.na(enrolled_subset2$newborn_visit)] %within% get_interval)
    schedule_by_week$six_month[week] = sum(enrolled_subset2$six_mo_visit[!is.na(enrolled_subset2$six_mo_visit)] %within% get_interval)
    schedule_by_week$twelve_month[week] = sum(enrolled_subset2$twelve_mo_visit[!is.na(enrolled_subset2$twelve_mo_visit)] %within% get_interval)
  }
  # calculate total visits per week
  schedule_by_week <- schedule_by_week %>%
    mutate(all_visits = newborn + six_month + twelve_month)
  
  # make a long version of data for plotting and tables
  schedule_long_form <- schedule_by_week %>%
    gather(key = "Visit_Type", value = "Number_of_Visits",
           -week_start, -week_end, -week_interval,-Week_Number) 
  
  # table
  visit_table <- schedule_long_form %>%
    filter(Visit_Type %in% input$visit_choice & 
             Week_Number <= input$num_weeks_choice) %>%
    mutate(week_start = as.character(week_start), 
           week_end = as.character(week_end))
  
  summary_by_visit <- visit_table %>%
    group_by(Visit_Type) %>%
    summarize(Number_of_Subjects = as.integer(N_to_model),
                Number_of_Visits = sum(Number_of_Visits, na.rm = T)) %>%
    select(Number_of_Subjects, Visit_Type, Number_of_Visits)
  
  summary_by_visit
})

}


# SHINY APP CALL --------------------------------------------------------------

shinyApp(ui = ui, server = server_function)
