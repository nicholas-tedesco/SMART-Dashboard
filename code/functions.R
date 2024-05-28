# packages 

library(dplyr)
library(httr)
library(tidyverse)
library(lubridate)

library(moonBook)
library(ggplot2)
library(ggiraph)
library(grDevices)
library(sjlabelled)



# google sheets 

saveData <- function(data, SHEET_ID) {
  # have to convert to dataframe
  data <- data %>% as.list() %>% data.frame()
  # add to sheet
  sheet_append(SHEET_ID, data)
}

loadData <- function(SHEET_ID) {
  # read data from sheet
  data.frame(read_sheet(SHEET_ID))
}


# REDCap

  get_rc <- function(form_id) {
    
    # purpose: pull data from REDCap using csv parameters, API url/token, and form_id
    # inputs:  form_id (ex: 'demographics')
    
    if(length(form_id) == 1){
      formData = list(
        token = custom_token, 
        content='record',
        action='export',
        format='csv',
        type='flat',
        csvDelimiter='',
        rawOrLabel='raw',
        rawOrLabelHeaders='raw',
        exportCheckboxLabel='false',
        exportSurveyFields='false',
        exportDataAccessGroups='false',
        returnFormat='csv', 
        'forms[0]' = form_id
      )
    } else if (length(form_id) == 2){
      formData = list(
        token = custom_token, 
        content='record',
        action='export',
        format='csv',
        type='flat',
        csvDelimiter='',
        rawOrLabel='raw',
        rawOrLabelHeaders='raw',
        exportCheckboxLabel='false',
        exportSurveyFields='false',
        exportDataAccessGroups='false',
        returnFormat='csv', 
        'forms[0]' = form_id[1], 
        'forms[1]' = form_id[2]
      )
    } else if(length(form_id) > 2){
      return('Error: form_id can only have up to two elements')
    }
    
    # retrieve response to API request
    response <- POST(
      url = custom_url, 
      body = formData
    )
    
    # obtain result from response
    result <- content(response)
    
    # output
    return(result)
    
  }
  
  
# treatment assignment 
  
  stage1 <- function() {

    # purpose of function is to assign patient to first stage of treatment
    
    # treatment stage 1 options
    treatments <- c(
      'IV Methylprednisolone 30mg BID', 
      'Upadacitinib 30mg BID', 
      'IV Methylprednisolone 30mg BID + Upadacitinib 45mg daily'
    )
    
    # random assignment
    sample(treatments, 1)
    
  }
  
  stage2 <- function(treat1) {
    
    # purpose of function is to assign patient to second stage of treatment, 
    # given first treatment as input
    
    # treatment stage 2 options (dependent on first stage)
    treatments <- c()
    
    if(treat1 == 'IV Methylprednisolone 30mg BID') {
      treatments <- c(
        'Add Cyclosporine Rescue', 
        'Add Upadacitinib 30mg BID Rescue'
      )
    } else if (treat1 == 'Upadacitinib 30mg BID') {
      treatments <- c(
        'Switch to IV Methylprednisolone 30mg BID + Cyclosporine', 
        'Add IV Methylprednisolone 30mg BID'
      )
    } else {
      treatments <- c(
        'Escalate to IV Methylprednisolone 30mg BID + Upadacitinib 30mg BID', 
        'Switch to Cyclosporine Rescue'
      )
    }
    
    # random assignment
    sample(treatments, 1)
    
  }
  
  assign_patients <- function(cohort_size) {
    
    # purpose of function is to generate treatment dataset (including stages 1 and 2) for given 
    # cohort size of patients
    
    data <- data.frame(patient_id = 1:cohort_size) %>%
      rowwise() %>%
      mutate(
        treatment_one = stage1(), 
        treatment_two = stage2(treatment_one)
      )
    
  }
  
  

# assessment data 
  
  assess_toDF <- function(assessData) {
    
    # purpose: calculate assessment times based on medication start
    # inputs: dataframe including patient id (patient_id), medication start time (start_time)
    
    current_time = Sys.time()
    
    assessData <- assessData %>%
      mutate(
        day_one = start_time + 24 * 60 * 60, 
        day_two = start_time + 48 * 60 * 60, 
        day_three = start_time + 72 * 60 * 60, 
        time_until_day_one = difftime(day_one, current_time, units = 'hours'), 
        time_until_day_two = difftime(day_two, current_time, units = 'hours'), 
        time_until_day_three = difftime(day_three, current_time, units = 'hours')
      )
    
    return(assessData)
    
  }
  

# symptoms + results
  
  symptom_plot <- function(id){
    
    plot1 <- test_snr %>%
      filter(stage == 1) %>%
      filter(PatientID == id) %>%
      pivot_longer(cols = contains('day'), names_to = 'time', values_to = 'CRP') %>%
      ggplot(aes(x = time, y = CRP, group = PatientID, color = PatientID)) + 
      geom_line() + 
      geom_point() + 
      theme_bw() + 
      theme(
        legend.position = 'bottom'
      )
    
    plot2 <- test_snr %>%
      filter(stage == 2) %>%
      filter(PatientID == id) %>%
      pivot_longer(cols = contains('day'), names_to = 'time', values_to = 'CRP') %>%
      ggplot(aes(x = time, y = CRP, group = PatientID, color = PatientID)) + 
      geom_line() + 
      geom_point() + 
      theme_bw() + 
      theme(
        legend.position = 'bottom'
      )
    
    ggplotly(subplot(plot1, plot2, nrows = 1))
    
  }
  
