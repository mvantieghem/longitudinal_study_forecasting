## Longitudinal Study Forecasting Tool 

Interactive tool for developmental researchers to improve longitudinal study design and implementation. \
shiny app: https://mvantieghem.shinyapps.io/longitudinal_study_forecasting/ \
Author: Michelle VanTieghem



### Getting started:
#### 1) Download template csv file
#### 2) Enter your cohort data into the template csv file.
**Note: all dates must be provided in yyyy-mm-dd format!**
**For pregnant women: child due date goes in the child birth date column.**
**Optional columns: demographic variables of interest, as a factor(e.g. Group 1, Group 2).
#### 3) Upload your data (csv file).
### Using the App
#### 1) Cohort Selection
Select the features of the cohort to be modeled on the left-hand panel , e.g. pregnant women only, or low income women.
Then, select the number of visits for your study, and the target ages of visits.
#### 2) Study Overview
Get summary information on selected cohort and number of participants per visit.
Visualize the volume of study visits over time.
#### 3) Visit Schedules
Select the visit type, start date, duration of schedules to be displayed.
Interactively view weekly schedules in different formats, and download the schedule.

### Code: 
app.R has code to run the interactive web application. 
