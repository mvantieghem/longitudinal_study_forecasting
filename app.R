# Shiny app for forcasting timing & volume of study visits.
# Author: Michelle VanTieghem
# Date: Aug 11, 2020

# set up environment and data ------------------------------------------------

library(shiny) # for interactive web app
library(tidyverse)# for cleaning data
library(ggplot2)# for making figures
library(shinythemes) # for making shiny app prettier
library(lubridate) # for date calculations
library(DT) # needed for interactive data tables!

set.seed(333) # for random sampling

#source("scripts/custom_functions.R") # custom functions

# set customizations
template_data <- read.csv("template_data.csv")
demo_data <- read.csv("data/enrolled_for_study_forecast.csv")
demo1_levels <- levels(demo_data$demo_variable1)
demo2_levels <- levels(demo_data$demo_variable2)
demo3_levels <- levels(demo_data$demo_variable3)
visit_list <- c("Visit1", "Visit2", "Visit3", "Visit4")
default_visit_ages = c(2, 26, 52, 78)
schedule_min <- 1
schedule_max <- 52


# USER INTERFACE -----------------------------------------------------
ui <- fluidPage(
    titlePanel("Longitudinal Study Forecasting Tool"), 

    sidebarLayout( # Sidebar panel for inputs ----
      sidebarPanel(
        h4("Cohort Selection"),
        fileInput(inputId = "file1",
                label = "Upload your data (csv file)",
                multiple = FALSE, 
                accept = c("text/csv", "test/comma-separated-values", "text/plain", ".csv"), 
                placeholder = "No file selected", 
                buttonLabel = "Browse..."), 
        checkboxGroupInput(inputId = 'status_choice',
                         label = "Current Maternal Status (estimated from child due date)",
                         choices = c("Pregnant", "New Mom"),
                         selected = c("Pregnant", "New Mom")),
        checkboxGroupInput(inputId='demo1_choice', 
                         label = "Demographic Variable 1",
                         choices = demo1_levels, 
                         selected = demo1_levels),
        checkboxGroupInput(inputId = 'demo2_choice', 
                       label = "Demographic Variable 2",
                       choices = demo2_levels,
                       selected = demo2_levels),
        sliderInput(inputId = "N_participant_choice",
                  label = "Proportion of selected cohort to do infant testing:",
                  min = 0, max = 1, value = 1),
        
         numericInput(inputId = "number_visits",
                                 label = "Number of visits",
                                 value = 3, min = 1, max = 4),
         numericInput(inputId = "Visit1_age",
                               label = "age of Vist1 (weeks)",
                               value = default_visit_ages[1], min = 0, max = 1000),
          numericInput(inputId = "Visit2_age",
                               label = "age of Visit2 (weeks)",
                               value = default_visit_ages[2], min = 0, max = 1000),
          numericInput(inputId = "Visit3_age",
                               label = "age of Visit3 (weeks)",
                               value =default_visit_ages[3], min = 0, max = 1000),
        numericInput(inputId = "Visit4_age", 
                     label = "age of Visit 4 (weeks)",
                     value = default_visit_ages[4], min = 0, max = 1000)
        ),

  # Main panel for displaying outputs ----
   mainPanel(
      # To make panel with tabs
      tabsetPanel(type = "tabs",
                  tabPanel("Instructions", 
                        fluidRow(
                           br(),
                           h4('Getting Started'), 
                           h5('1) Download template csv file:'), 
                           downloadButton(outputId = 'downloadTemplate', "Download"),
                           h5('2) Enter your cohort data into the template csv file.'),
                           HTML('<p><i> Note: all dates must be provided in yyyy-mm-dd format! </i>'),
                           HTML('<p><i>For pregnancy cohorts, child due date goes in the child birth date column.</i>'),
                           p('Optional columns: demographic variables of interest, as a factor (e.g. Income >60K vs. <60K)'),
                           h5('3) Upload your data (csv file).'), 
                           p("___________________________________________")), 
                        fluidRow(
                           h4('Using the App'), 
                           h5('1) Cohort Selection'),
                           p('Select the features of the cohort to be modeled on the left-hand panel,
                           e.g. pregnant women only, or low income women'),
                           p('Then, select the number of visits for your study, and the target ages of visits.'),
                           h5('2) Study Overview'),
                           p('Get a summary table with total numbers of participants, by type of study visit & visualize the volume of study visits over time for the selected cohort.'),
                           h5('3) Visit Schedules' ),
                           p ('Select the visit type, start date, duration of schedules to be displayed.'),
                           p('Interactively view weekly schedules in different formats, and download the schedule.')),
                        fluidRow(
                          HTML("<h5> For questions, contact the author: <a href= 'https://mvantieghem.github.io/'> Michelle VanTieghem</a>"),
                          HTML("<h5> For code, see the <a href = 'https://www.github.com/mvantieghem/longitudinal_study_forecasting/'> Github repository</a>"))),
                  tabPanel("Study Overview",
                           fluidRow(
                             column(3, h4("Total Participants:")), 
                             column(3, verbatimTextOutput(outputId = 'total_N'))),
                           h4("Total visits"),
                           tableOutput(outputId = "visit_totals"),
                           h4("N by demographic variables"),
                            tableOutput(outputId = "demo_totals"),
                            h4("Visualize volume of study visits over time"),
                              plotOutput(outputId = "plot_weekly_visits")), 
                  tabPanel("Visit Schedules", 
                           fluidRow(
                             br(),
                             h3("Select visit type and schedule"),
                             br()),
                           fluidRow(
                             column(6, selectInput(inputId = "select_schedule", 
                                                 label = "Schedule format",
                                                choices = list("Weekly Visit Totals" = 1,
                                                               "Participant-specific" = 2),
                                                selected = 1)),
                             column(6, dateInput(inputId ='start_date', 
                                                 label = 'Start Date',
                                                 value =  Sys.Date(), 
                                                 format = 'mm/dd/yyyy'))),
                            fluidRow( 
                              column(6, checkboxGroupInput(inputId = 'visit_choice', 
                                                 label = 'Visits to be displayed', 
                                                 choices = visit_list,
                                                 selected = visit_list)),
                               column(6,  sliderInput(inputId = 'num_weeks_choice', 
                                       label = 'Duration of schedule (weeks)',
                                       min = schedule_min,
                                       max = schedule_max, 
                                       value =  4))),
                           fluidRow(
                             dataTableOutput(outputId = "visit_schedule"),
                              h4("Download schedule"), 
                              downloadButton(outputId = 'download_visit_sched', "Download"))
                         )
                   )
      )
    )
)
    




# SERVER FUNCTION ---------------------------------------------------------
server_function <- function(input, output, session) {

  # DOWNLOADING THE TEMPLATE CSV------------
  output$downloadTemplate <-  downloadHandler(
    filename = function() {
      paste('template_data.csv', sep='')
    }, 
    content = function(file){
      write.csv(template_data, file, row.names = F)
    })
  
  # SET UP EMPTY SCHEDULES  ------------------
   schedule_by_week <-  reactive({
      schedule_by_week <- data.frame(week_number = schedule_min:schedule_max) %>%
      mutate(week_start = ymd(input$start_date) + weeks(0:(schedule_max-1)),
             week_end = week_start + days (6),
             week_interval = interval(week_start, week_end))
    return(schedule_by_week)
  })
  
   # UPLOAD USER DATA ---------------------------
   load_data <- reactive({
     if (is.null(input$file1)) {
       # if there is no input, use our template data.
       read.csv("data/enrolled_for_study_forecast.csv")
       #read.csv("data/test_data.csv")
     } else{ 
       read.csv(input$file1$datapath)
     }
   })
   
   # update inputs based on uploaded data.
   observe({
       new_levels1 <- levels(load_data()$demo_variable1)
       # Can use character(0) to remove all choices
       if (is.null(new_levels1))
         new_levels1 <- character(0)
       
       updateCheckboxGroupInput(
         session, inputId = "demo1_choice", 
         choices = new_levels1, selected = new_levels1)
   })
     
    observe({
       new_levels2 <- levels(load_data()$demo_variable2)
       if (is.null(new_levels2))
         new_levels2 <- character(0)
       
       updateCheckboxGroupInput(
         session, inputId = "demo2_choice",
         choices = new_levels2, selected = new_levels2)
   })

    # add an observe marker for number of visits. 
    observe({
      new_visit_ages = default_visit_ages
      if (input$number_visits  == 1){
        new_visit_list = c("Visit1")
        new_visit_ages[2:4] <- NA
      } else if (input$number_visits == 2){
        new_visit_list = c("Visit1", "Visit2")
        new_visit_ages[3:4] <- NA
      } else if (input$number_visits == 3){
        new_visit_list = c("Visit1", "Visit2", "Visit3")
        new_visit_ages[4] <- NA
      } else if(input$number_visits == 4){
        new_visit_list = c("Visit1", "Visit2", "Visit3", "Visit4")
      }
      
      # update the visit choice for schedule displays
      updateCheckboxGroupInput(
        session, inputId = "visit_choice", 
        choices = new_visit_list, selected = new_visit_list)
      
      # update the boxes for the visit ages 
      # note: don't need to update visit 1 
      # because impossible for Visit1 to be NA because min visit  # = 1
      updateNumericInput(
        session, inputId = "Visit2_age",
        value = new_visit_ages[2])
      updateNumericInput(
        session, inputId = "Visit3_age",
        value = new_visit_ages[3])
      updateNumericInput(
        session, inputId = "Visit4_age",
        value = new_visit_ages[4])
    })
   
  # FORMAT COHORT DATA ------------------------
   sample_data <- reactive({
      my_data <- load_data() %>%
         mutate(child_birth_date = ymd(child_birth_date),
             status = as.factor(ifelse(child_birth_date > Sys.Date(), "Pregnant", "New Mom")),
              Visit1 = child_birth_date + weeks(input$Visit1_age),
              Visit2 = child_birth_date + weeks(input$Visit2_age),
              Visit3 =  child_birth_date + weeks(input$Visit3_age),
              Visit4 = child_birth_date + weeks(input$Visit4_age))
      
      # filiter data based on whether demo variable exists or not. 
      if (!is.na(my_data$demo_variable1) & !is.na(my_data$demo_variable2)){
       clean_data <- my_data %>%
            filter(demo_variable1 %in% input$demo1_choice & 
                 demo_variable2 %in% input$demo2_choice &
                 status %in% input$status_choice)
      } else if (!is.na(my_data$demo_variable1) & is.na(my_data$demo_variable2)){
        clean_data <- my_data %>%
            filter(demo_variable1 %in% input$demo1_choice & 
                 status %in% input$status_choice)
      } else if (is.na(my_data$demo_variable1) & is.na(my_data$demo_variable2)){
        clean_data <- my_data %>%
            filter(status %in% input$status_choice)
      } else if(is.na(my_data$demo_variable1) & !is.na(my_data$demo_variable2)){
        clean_data <- my_data %>%
          filter(status %in% input$status_choice &
                   demo_variable2 %in% input$demo2_choice)
      }

    # get random sample of subjects based on N 
    N_to_model <- round(nrow(clean_data) * input$N_participant_choice)
    sample_data <- clean_data %>%
       sample_n(N_to_model)
    return(sample_data)
  })
  
  # convert into long format with schedule   
  long_data <- reactive ({
    long_data <- sample_data() %>%
      gather(key = visit, value = visit_date, -child_birth_date, -survey_date, -subject_id, 
             -starts_with("demo"), -status) %>%
      mutate(visit_number = as.numeric(ifelse(visit == "Visit1", 1,
                                              ifelse(visit == "Visit2", 2,
                                            ifelse(visit == "Visit3", 3, 
                                               ifelse(visit == "Visit4", 4, NA))))),
              week_number = NA, week_interval = NA, month_number = NA) %>%
      filter(visit_number <= input$number_visits) # only keep the visits needed.

    # get week and date range for each visit 
    for (i in 1:nrow(long_data)){
      get_date = long_data$visit_date[i]
      if (sum(get_date %within%  schedule_by_week()$week_interval > 0)){
        week_num_index <- which(get_date %within% schedule_by_week()$week_interval)
        long_data$week_number[i]  <-  week_num_index
        long_data$week_interval[i] <- as.character(schedule_by_week()$week_interval[week_num_index])
      } else{
        long_data$week_number[i]   <- NA
        long_data$week_interval[i] <- NA
      }
    }
    return(long_data)
  })
  
  # calculate visit totals by week
    weekly_sums  <- reactive({
      weekly_sums <- long_data() %>%
        filter(!is.na(week_number)) %>%
        group_by(week_number, week_interval, visit) %>%
        count() %>%
        rename(number_of_visits  = n) 
     return(weekly_sums)
  })
    
  
    # STUDY OVERVIEW - N totals -----------------------
   output$total_N <- renderText({
     nrow(sample_data())
   })

    # STUDY OVERVIEW - visit totals ------------------------
    output$visit_totals <- renderTable({
       long_data() %>%
          filter(!is.na(week_number)) %>%
          group_by(visit) %>%
          count() %>%
          rename(number_of_visits  = n) 
    }, rownames = F)
    
    ## STUDY OVERVIEW - demo totals --------------------
    output$demo_totals <- renderTable({
        df <- sample_data() 
        if (!is.na(df$demo_variable1) & !is.na(df$demo_variable2)){
            subs_demo <- df %>%
              group_by(status, demo_variable1, demo_variable2) %>%
              summarize(n = n())
        } else if(!is.na(df$demo_variable1) & is.na(df$demo_variable2)){
          subs_demo <- df %>%
            group_by(status, demo_variable1) %>%
            summarize(n = n())
        }  else if(is.na(df$demo_variable1) & is.na(!df$demo_variable2)){
          subs_demo <- df %>%
            group_by(status, demo_variable2) %>%
            summarize(n = n())
        } else if(is.na(df$demo_variable1) & is.na(df$demo_variable2)){
          subs_demo <- df %>%
            group_by(status) %>%
            summarize(n = n())
        }
      subs_demo }, rownames = F)
  
  ## STUDY OVERVIEW - PLOT VISITS BY WEEK --------------------
  output$plot_weekly_visits <- renderPlot({
   weekly_sums() %>%
      ggplot(aes(x = week_number, y = number_of_visits)) + 
      facet_wrap(~visit, nrow = 4) +
      geom_line() + theme_bw() + ylab ("Number of Study Visits") + 
      xlab ("Week of Study") 
  })
  
  # SCHEDULE CHOICES ------------------------------
    weekly_sched <- reactive({
      req(weekly_sums)
      weekly_sched <- weekly_sums() %>%
        filter(visit %in% input$visit_choice & 
               week_number <= input$num_weeks_choice)
       return(weekly_sched)
    })

    participant_sched <- reactive({
      req(long_data)
      participant_sched <- long_data() %>%
        filter(visit %in% input$visit_choice & 
                 week_number <= input$num_weeks_choice) %>%
        select(week_number, week_interval, visit_date, visit, subject_id)
      return(participant_sched)
    })
    
    which_schedule <- reactive({
      if (input$select_schedule == 1){
        weekly_sched()
      } else{
        participant_sched()
      }
    })
    
   # display interactive table
   output$visit_schedule <- DT::renderDataTable({
      which_schedule() }, rownames = F)
  
  # download button
  output$download_visit_sched <-downloadHandler(
    filename = function() {
      paste('Weekly_schedule-', Sys.Date(), '.csv', sep='')
    }, 
    content = function(file){
      write.csv(which_schedule(), file, row.names = F)
    })

    
}
  

# SHINY APP CALL --------------------------------------------------------------

shinyApp(ui = ui, server = server_function)

