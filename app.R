# Shiny app for forcasting timing & volume of study visits.
# Author: Michelle VanTieghem
# Date: June 15, 2020

# set up environment and data ------------------------------------------------

library(shiny) # for interactive web app
library(tidyverse)# for cleaning data
library(ggplot2)# for making figures
library(shinythemes) # for making shiny app prettier
library(lubridate) # for date calculations
library(DT) # needed for interactive data tables!

set.seed(333) # for random sampling
# custom functions
source("scripts/custom_functions.R")

# open, and clean your data automatically by running these scripts:
#source_rmd("scripts/data_setup_script.Rmd")

visit_list <- c('newborn', 'six_month', 'twelve_month', 'all_visits')

# USER INTERFACE -----------------------------------------------------

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # App title ----
  titlePanel("Longitudinal Study Forecasting Tool"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      fileInput(inputId = "file1",
                label = "Upload CSV file with enrollment data and birth dates",
                multiple = FALSE, 
                accept = c("text/csv", "test/comma-separated-values", "text/plain", ".csv"), 
                placeholder = "No file selected", 
                buttonLabel = "Browse..."), 
      checkboxGroupInput(inputId='enrollment_choice', 
                         label = "Enrollment Date",
                         choices = c("April", "May", "June", "July"), 
                         selected =c("April", "May", "June", "July")),
      checkboxGroupInput(inputId = 'status_choice',
                         label = "Current Maternal Status",
                         choices = c("Pregnant", "New Mom"),
                         selected = c("Pregnant", "New Mom")),
      checkboxGroupInput(inputId = 'enrichment_choice', 
                         label = "Enrichment Variable",
                        choices = c("< 60K", "> 60K"),
                         selected = c("< 60K", "> 60K")),
      sliderInput(inputId = "N_participant_choice",
                  label = "Ratio of cohort for infant testing:",
                  min = 0, max = 1, value = 0.5),
      checkboxGroupInput(inputId = 'visit_choice', 
                         label = 'Visit Type (Target Age)', 
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
                  tabPanel("About", 
                           br(),
                           h4('Interactive tool for developmental researchers'),
                           h5(' To improve longitudinal study design and implementation'), 
                             p(""),
                             p("Author: Michelle VanTieghem"), 
                             HTML("<p> For code, see the <a href = 'https://www.github.com/mvantieghem/longitudinal_study_forecasting/'> Github repository</a>"),
                           br(), 
                            p("___________________________________________"), 
                           br(),
                           h4('Instructions for users'), 
                           h5('1) Provide input csv file.'),
                             p('Required columns: baseline_date, child_birth_date.'),
                              HTML('<p><i>For pregnancy cohorts, include child due date in the child birth date column.</i>'),
                             p('Optional columns: enrichment variable of interest, as a factor (e.g. Income >60K vs. <60K)'),
                           h5('2) Select inputs on side bar to modulate variables of the study design'),
                              p('For example, filter the sample for a particular enrichment variable of interest.'),
                             p('OR choose a sepcific age-target for study visits (e.g. six months of age'),
                           h5('3) Look at the output to see how decisions in study design influence the projected schedule.'),
                            p('1. Summary table with total numbers of participants, by type of study visit'),
                            p('2. Figures to visualize volume of study visits over time, by type of study visit.'),
                              p('3. Dynamic table sto view & filter weekly schedules for planning study operations.')),

                           
                  tabPanel("Summary", tableOutput(outputId = "summary_table")),
                  tabPanel("Figures", plotOutput(outputId = "plot_visits")), 
                  tabPanel("Tables", dataTableOutput(outputId = "visit_table"))
                  
      )
    )
  )
)


# SERVER FUNCTION ---------------------------------------------------------

server_function <- function(input, output, session) {
  ## FUNCTION FOR CHECKING IF DATA EXISTS -------------------------

  ## FUNCTION FOR OPENING DATA -------------------------
  load_data <- reactive({
    if (is.null(input$file1)) {
      # if there is no input, use our template data.
      read.csv("data/enrolled_for_study_forecast.csv")
    } else{ 
      read.csv(input$file1$datapath)
    }
  })
  ## FUNCTION FOR CLEANING DATA -------------------------
  clean_data <- reactive({
    my_data <- load_data() %>%
      # make sure you're only including valid data
      filter(as.Date(baseline_date) <= Sys.Date()) %>%
      # caldulate the month of enrollment...
      mutate(baseline_date = as.Date(baseline_date), 
             enrollment_month = as.factor(ifelse(baseline_date < "2020-04-01", "March",
                                                 ifelse(baseline_date < "2020-05-01", "April",
                                                        ifelse(baseline_date < '2020-06-01', "May", 
                                                               ifelse(baseline_date < '2020-07-01', "June",
                                                                      ifelse(baseline_date < "2020-08-01", "July", 
                                                                             ifelse(baseline_date < "2020-09-01", "August", 
                                                                                    ifelse(baseline_date < "2020-10-01", "September", NA)))))))),
             # if child's birth date is less than today, they are still pregnant.
             status = as.factor(ifelse(as.Date(child_birth_date) > Sys.Date(), "Pregnant", "New Mom")),
             child_birth_date = ymd(child_birth_date))
    
    #PROCESS DATA BASED ON INPUT SELECTIONS
    my_data_subset <- my_data %>%
      mutate(newborn_visit =child_birth_date,
             six_mo_visit =  child_birth_date + months(6),
             twelve_mo_visit =  child_birth_date + years(1)) %>% 
      # filter based on enrollment dates chosen and pregnant vs. new mom.
      filter(enrollment_month %in% input$enrollment_choice & 
               status %in% input$status_choice & 
               enrichment_variable %in% input$enrichment_choice)
    
    return(my_data_subset)
 
      ## FUNCTIONS TO UPDATE INPUTS BASED ON VARIABLES IN UPLOADED DATA --------
      # note: these observe () functions must be placed *inside* the reactive() function
     # then they will be updated whenever new data is uploaded.
        observe({
          updateCheckboxGroupInput(
            session, inputId = "enrollment_choice", 
            choices = levels(clean_data()$enrollment_month)
          )
        })
        
        observe({
          updateCheckboxGroupInput(
            session, inputId = "enrichment_choice",
            choices = levels(clean_data()$enrichment_variable)
           # selected = levels(clean_data()$enrichment_variable)
          )
        })
})    
  
  ## FUNCTION TO CALCULATE NUMBER OF PARTICIPANTS IN SAMPLE BASED ON INPUT -------------- 
  N_to_model <- reactive({
    # require the data to be cleaned first
    req(clean_data())
    # count how many subjects to model for follow-up visits based on input 
    N_to_model <- round(nrow(clean_data()) * input$N_participant_choice)
    return(N_to_model)
  })
  
  ## FUNCTION TO CALCULATE VISIT SCHEDULE FOR SAMPLE BASED ON INPUT ---------------------- 
  schedule <- reactive({
    # require N to be calculated and data to be cleaned 
    req(N_to_model())
    req(clean_data())
    
    # get random sample of subjects based on N 
    sample <- clean_data() %>%
      # get random sample from enrolled sample
      sample_n(N_to_model())
    
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
      schedule_by_week$newborn[week] = sum(sample$newborn_visit[!is.na(sample$newborn_visit)] %within% get_interval)
      schedule_by_week$six_month[week] = sum(sample$six_mo_visit[!is.na(sample$six_mo_visit)] %within% get_interval)
      schedule_by_week$twelve_month[week] = sum(sample$twelve_mo_visit[!is.na(sample$twelve_mo_visit)] %within% get_interval)
    }
    # calculate total visits per week
    schedule_by_week <- schedule_by_week %>%
      mutate(all_visits = newborn + six_month + twelve_month)
    
    
    # make a long version of data for plotting and tables
    schedule_long_form <- schedule_by_week %>%
      gather(key = "Visit_Type", value = "Number_of_Visits",
             -week_start, -week_end, -week_interval,-Week_Number) 
    # return the processed data 
    return(schedule_long_form)
    
  })
  
  
  ## FUNCTION FOR INTERACTIVE PLOT --------------------
  output$plot_visits <- renderPlot({
    # require that data schedule is available 
    req(schedule())
   # plot
    plot_visits <- schedule() %>%
      filter(Visit_Type %in% input$visit_choice & 
               Week_Number <= input$num_weeks_choice) %>%
      ggplot( aes(x = Week_Number, y = Number_of_Visits)) + 
      facet_wrap(~Visit_Type, nrow = 4) + 
      geom_line() + theme_bw() + ylab ("Number of Study Visits") + 
      xlab ("Week of Study")
    
    # Display the plot with all of the choices incorporated
    plot_visits
    
  })
  # FUNCTION FOR INTERACTIVE TABLE ------------------------------
  output$visit_table <- DT::renderDataTable({
    # require that data schedule is available 
    req(schedule())
    # table
  visit_table <- schedule() %>%
      filter(Visit_Type %in% input$visit_choice & 
              Week_Number <= input$num_weeks_choice) %>%
      mutate(week_start = as.character(week_start), 
             week_end = as.character(week_end)) %>%
    select(-week_interval)
  visit_table
  })

# FUNCTION FOR INTERACTIVE SUMMARY TABLE ------------------------------
output$summary_table <- renderTable({
  # require that data schedule is available, and Number o participants
  req(schedule())
  req(N_to_model())
  # make summary table from schedule 
  visit_table <- schedule() %>%
    filter(Visit_Type %in% input$visit_choice & 
             Week_Number <= input$num_weeks_choice) %>%
    mutate(week_start = as.character(week_start), 
           week_end = as.character(week_end))
  
  summary_by_visit <- visit_table %>%
    group_by(Visit_Type) %>%
    summarize(Number_of_Subjects = as.integer(N_to_model()),
                Number_of_Visits = sum(Number_of_Visits, na.rm = T)) %>%
    select(Number_of_Subjects, Visit_Type, Number_of_Visits)
  
  summary_by_visit
})

}


# SHINY APP CALL --------------------------------------------------------------

shinyApp(ui = ui, server = server_function)
