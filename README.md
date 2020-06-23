## Longitudinal Study Forecasting Tool 

Interactive tool for developmental researchers to improve longitudinal study design and implementation. \
shiny app: https://mvantieghem.shinyapps.io/longitudinal_study_forecasting/ \
Author: Michelle VanTieghem

### Contents: 
app.R has code to run the interactive web application. 

### Input of application:
Data provided by user, which sets variable inputs.
Default dataset provided from the COPE Study cohort of the Baby Bees Lab, NYU Langone.

### Instructions for users: 
#### 1) Provide input csv file. 
Required columns: baseline_date, child_birth_date. \
*For pregnancy cohorts, include child due date in the child birth date column* \
Optional columns: enrichment variables of interest,as a factor (e.g. Income > 60K or < 60K)
#### 2) Select inputs on side bar to modulate variables of the study design
For example, filter the sample for particular enrichment variable of interest \
OR choose a specific age-target for study visit (e.g. six months of age)
#### 3) Look at the output to see how decisions in study design influence the projected study schedule.
1) Summary table with total numbers of participants, by type of study visit. 
2) Figures to visualize volume of study visits over time, by type of study visit 
3) Dynamic tables to view & filter weekly schedules for planning study operations.
