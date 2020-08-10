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

visit_list <- c('newborn', 'six_month', 'twelve_month')

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
                         label = "Month of Survey Enrollment",
                         choices = c("April", "May", "June", "July"), 
                         selected =c("April", "May", "June", "July")),
      checkboxGroupInput(inputId = 'status_choice',
                         label = "Maternal Status",
                         choices = c("Pregnant", "New Mom"),
                         selected = c("Pregnant", "New Mom")),
      checkboxGroupInput(inputId = 'demo1_choice', 
                         label = "Demographic Variable 1",
                         choices = c("< 60K", "> 60K"),
                         selected = c("< 60K", "> 60K")),
      checkboxGroupInput(inputId = 'demo2_choice', 
                         label = "Demographic Variable 2",
                         choices = c("White", "Other"),
                         selected = c("White", "Other")),
      sliderInput(inputId = "N_participant_choice",
                  label = "Proportion of selected cohort to do infant testing:",
                  min = 0, max = 1, value = 0.5),
      checkboxGroupInput(inputId = 'visit_choice', 
                         label = 'Visit Type (Target Age)', 
                         choices = visit_list,
                         selected = visit_list),
      dateInput(inputId ='start_date', 
                label = 'Start Date of Infant Visits',
                value = '2020-08-01', format = 'mm/dd/yyyy'),
      sliderInput(inputId = 'num_weeks_choice', 
                  label = 'Duration of Schedule Displayed (in weeks)',
                  min = 3, max = 200, 
                  value =  10)
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
                             p('Required columns: enrollment_date, child_birth_date.'),
                             
                              HTML('<p><i> Note: all dates must be provided in yyyy-mm-dd format! </i>'),
                           HTML('<p><i>For pregnancy cohorts, child due date goes in the child birth date column.</i>'),
                             p('Optional columns: demographic variable of interest, as a factor (e.g. Income >60K vs. <60K)'),
                           h5('2) Select inputs on side bar to modulate variables of the study design'),
                              p('For example, choose a specific age-target for study visits (e.g. six months of age)'),
                           p('And choose a specific subset of data based on demographic variable (e.g. < 60K)'),
                           h5('3) Look at the output to see how decisions in study design influence the projected schedule.'),
                            p('1. Summary table with total numbers of participants, by type of study visit'),
                            p('2. Figures to visualize volume of study visits over time, by type of study visit.'),
                              p('3. Dynamic tables to view & filter weekly schedules for planning study operations.')),
               tabPanel("Summary", tableOutput(outputId = "summary_table"),
                        tableOutput(outputId= "totals_table")),
                  tabPanel("Figures", plotOutput(outputId = "plot_visits")), 
                  tabPanel("Tables", dataTableOutput(outputId = "visit_table"),
                           downloadButton(outputId = 'downloadData', "Download"))
                  
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
      read.csv("data/test_enrolled_for_study_forecast.csv")
      #read.csv("data/test_data.csv")
    } else{ 
      read.csv(input$file1$datapath)
    }
  })
  ## FUNCTION FOR CLEANING DATA -------------------------
  clean_data <- reactive({
      my_data <- load_data() %>%
     # my_data <- read.csv("data/enrolled_for_study_forecast.csv") %>%
      # make sure you're only including valid data
      filter(ymd(enrollment_date) <= Sys.Date()) %>%
      # caldulate the month of enrollment...
      mutate(enrollment_date = ymd(enrollment_date), 
             enrollment_month = as.factor(ifelse(enrollment_date < "2020-04-01", "March",
                                                 ifelse(enrollment_date < "2020-05-01", "April",
                                                        ifelse(enrollment_date < '2020-06-01', "May", 
                                                               ifelse(enrollment_date < '2020-07-01', "June",
                                                                      ifelse(enrollment_date < "2020-08-01", "July", 
                                                                             ifelse(enrollment_date < "2020-09-01", "August", 
                                                                                    ifelse(enrollment_date < "2020-10-01", "September", NA)))))))),
             # if child's birth date is less than today, they are still pregnant.,
             child_birth_date = ymd(child_birth_date),
             status = as.factor(ifelse(child_birth_date > Sys.Date(), "Pregnant", "New Mom")))
    
    #PROCESS DATA BASED ON INPUT SELECTIONS
    my_data_subset <- my_data %>%
      # Note: this is where I'd need to change things for selecting different visits 
      mutate(newborn_visit = child_birth_date,
             six_mo_visit =  child_birth_date + months(6),
             twelve_mo_visit =  child_birth_date + years(1)) %>% 
      # filter based on enrollment dates chosen and pregnant vs. new mom.
      filter(enrollment_month %in% input$enrollment_choice & 
               status %in% input$status_choice & 
               demo_variable1 %in% input$demo1_choice & 
               demo_variable2 %in% input$demo2_choice)
    
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
            session, inputId = "demo1_choice",
            choices = levels(clean_data()$demo_variable1)
          )
        })
        
        observe({
          updateCheckboxGroupInput(
            session, inputId = "demo2_choice", 
            choices = levels(clean_data()$demo_variable2)
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
      mutate(week_start = ymd(input$start_date) + weeks(0:199),
             week_end = ymd(input$start_date) + weeks(0:199) + days (7),
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
   # schedule_by_week <- schedule_by_week %>%
    #  mutate(all_visits = newborn + six_month + twelve_month)
    
    # make a long version of data for plotting and tables
    schedule_long_form <- schedule_by_week %>%
      gather(key = "Visit_Type", value = "Number_of_Visits",
             -week_start, -week_end, -week_interval,-Week_Number)
    # how can i add a level here?
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
  # FUNCTION FOR INTERACTIVE VISIT TABLE ------------------------------
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
  }, rownames = F)
  

## FUNCTION FOR DOWNLOADING THE VISIT TABLE ------
output$downloadData <-downloadHandler(
  filename = function() {
    paste('data-', Sys.Date(), '.csv', sep='')
  }, 
  content = function(file){
    # pull the reactive schedule, and export it.
    visit_table <- schedule() %>%
      filter(Visit_Type %in% input$visit_choice & 
               Week_Number <= input$num_weeks_choice) %>%
      mutate(week_start = as.character(week_start), 
             week_end = as.character(week_end)) %>%
      select(-week_interval)
    write.csv(visit_table, file)
  }
)


# FUNCTION FOR INTERACTIVE SUMMARY TABLE ------------------------------
output$summary_table <- renderTable({
  # require that data schedule is available, and Number o participants
  req(schedule())
  req(N_to_model())
  # filter based on inputs
  visit_table <- schedule() %>%
    filter(Visit_Type %in% input$visit_choice & 
             Week_Number <= input$num_weeks_choice) %>%
    mutate(week_start = as.character(week_start), 
           week_end = as.character(week_end))
  # make table 
  summary_by_visit <- visit_table %>%
    group_by(Visit_Type) %>%
    summarize(Number_of_Subjects = as.integer(N_to_model()),
              Subjects_per_Visit = sum(Number_of_Visits, na.rm = T)) %>%
    select(Visit_Type, Subjects_per_Visit)
  summary_by_visit

})

# FUNCTION FOR TOTALS TABLE
output$totals_table <- renderTable({
  # require that data schedule is available, and Number o participants
  req(schedule())
  req(N_to_model())
  # fitler based on inputs 
  visit_table <- schedule() %>%
    filter(Visit_Type %in% input$visit_choice & 
             Week_Number <= input$num_weeks_choice) %>%
    mutate(week_start = as.character(week_start), 
           week_end = as.character(week_end))
  # get totals.
  summary_totals <- visit_table %>%
    summarize(Total_Subjects = as.integer(N_to_model()),
              Total_Visits = sum(Number_of_Visits, na.rm = T))
  
  summary_totals
})

}


# SHINY APP CALL --------------------------------------------------------------

shinyApp(ui = ui, server = server_function)
