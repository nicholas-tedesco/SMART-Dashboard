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
  
  snr <- data.frame(
    PatientID = 1:50, 
    treatment = sample(c('Methylprednisolone', 'Cyclosporine', 'Third Med'), 50, replace = TRUE),
    stage = sample(1:2, 50, replace = TRUE), 
    day1 = rnorm(50, mean = 7, sd = 2), 
    day2 = rnorm(50, mean = 7, sd = 2), 
    day3 = rnorm(50, mean = 7, sd = 2), 
    response = sample(c('Y', 'N', 'U'), 50, replace = TRUE)
  ) %>% mutate(
    response = factor(response, levels = c('U', 'N', 'Y'))
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
    month = c(rep('Jan 2023', 3), rep('Feb', 3), rep('March', 3), rep('April', 3), rep('May', 3)), 
    type = rep(c('Screened', 'Approached', 'Enrolled'), 5), 
    count = rpois(15, 30)
  ) %>%
    mutate(
      month = factor(month, levels = c('Jan 2023', 'Feb', 'March', 'April', 'May')), 
      type = factor(type, levels = c('Screened', 'Approached', 'Enrolled'))
    )
  
  ## treatment ----
  set.seed(25324)             # set seed for reproducibility
  treatmentData <- assign_patients(62)
  
  ## assessment ---- 
  
  datetime_sequence <- seq(
    as.POSIXct('2023/04/30'),  # Create sequence of dates
    as.POSIXct('2023/05/02'),
    by = "10 mins")
  
  start_times <- sample(datetime_sequence, 62)
  
  assessData <- data.frame(
    patient_id = 1:62, 
    start_time = start_times
  )

  
  
  
  
  
  



