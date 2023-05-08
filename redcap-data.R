# README -----------------------------------------------------------------
# ========================================================================

# redcap-data.R
# purpose: pull and organize data from REDCap

# init NT 04/11/2023
# updated NT 05/01/2023

# to-do

  # organize data from REDCap in meaningful way
  # link REDCap data to dashboard



# setup ------------------------------------------------------------------
# ========================================================================

library(tidyr)

source('secret-data.R')
source('functions.R')
  


# data -------------------------------------------------------------------
# ========================================================================

  ## pull data from REDCap -------------------------------------
  ## -----------------------------------------------------------

  demo        <- get_rc('demographics')
  eligibilty  <- get_rc('eligibility_requirements')
  treatment   <- get_rc('drug_exposure_dosing')
  crp         <- get_rc('creactive_protein_crp')
  
  
  ## organize for dashboard ------------------------------------
  ## -----------------------------------------------------------
  
  
  
# test data (until REDCap is finalized) ----------------------------------
# ========================================================================
  
  ## symptoms and response ----
  
  test_snr <- data.frame(
    PatientID = c(1:50, 1:50), 
    treatment = sample(c('Methylprednisolone', 'Upadacitinib', 'Methylpred + Upa'), 100, replace = TRUE),
    stage = c(rep(1, 50), rep(2, 50)), 
    day0 = rnorm(100, mean = 9, sd = 1), 
    day1 = rnorm(100, mean = 8, sd = 2), 
    day2 = rnorm(100, mean = 7, sd = 2), 
    day3 = rnorm(100, mean = 7, sd = 2), 
    response = sample(c('Y', 'N', 'U'), 100, replace = TRUE)
  ) %>% mutate(
    response = factor(response, levels = c('U', 'N', 'Y'))
  )
  
  test_responseData1 <- data.frame(
    response_status = c('Responder', 'In-Progress', 'Non-Responder'),
    value = c(13, 10, 30)
  ) %>%
    mutate(
      response_status = factor(response_status, levels = c('Non-Responder', 'In-Progress', 'Responder'))
    )
  
  test_responseData2 <- data.frame(
    response_status = c('Responder', 'In-Progress', 'Non-Responder'),
    value = c(4, 18, 8)
  ) %>%
    mutate(
      response_status = factor(response_status, levels = c('Non-Responder', 'In-Progress', 'Responder'))
    )
  
  ## treatment distribution ----
  
  set.seed(66123)
  test_treatment <- assign_patients(45)
  
  ## enrollment ----
  
  test_excludedEligibility <- 200
  test_excludedIC <- 60
  test_enrolledCount <- 32
  test_totalScreened <- test_excludedEligibility + test_excludedIC + test_enrolledCount

  test_enrolledRate <- test_enrolledCount / test_totalScreened
  test_excludedEligibilityRate <- test_excludedEligibility / test_totalScreened
  test_excludedICRate <- test_excludedIC / test_totalScreened
  
  test_exclusionData <- data.frame(
    reason = c('Reason 1', 'Reason 2', 'Reason 3', 'Reason 4', 'Reason 5', 'Other'),
    reason_class = c('Class 1', 'Class 2', 'Class 3', 'Class 2', 'Class 1', 'Class 2'), 
    count = c(4, 14, 20, 32, 5, 10)
  )
  
  test_monthlyEnrollment <- data.frame(
    month = c(rep('Jan 2023', 3), rep('Feb', 3), rep('March', 3), rep('April', 3), rep('May', 3), rep('June', 3), rep('July', 3)), 
    type = rep(c('Screened', 'Approached', 'Enrolled'), 7), 
    count = c(50, 13, 5, 30, 5, 3, 70, 10, 8, 40, 12, 6, 25, 6, 3, 40, 15, 10, 39, 14, 3)
  ) %>%
    mutate(
      month = factor(month, levels = c('Jan 2023', 'Feb', 'March', 'April', 'May', 'June', 'July')), 
      type = factor(type, levels = c('Screened', 'Approached', 'Enrolled'))
    )
  
  ## treatment ----
  set.seed(25324)             # set seed for reproducibility
  treatmentData <- assign_patients(62)
  
  ## assessment ---- 
  
  datetime_sequence <- seq(
    as.POSIXct('2023/05/05'),  # Create sequence of dates
    as.POSIXct('2023/05/09'),
    by = "10 mins")
  
  start_times <- sample(datetime_sequence, 62)
  
  assessData <- data.frame(
    patient_id = 1:62, 
    start_time = start_times
  )

  
  
  
  
  
  



