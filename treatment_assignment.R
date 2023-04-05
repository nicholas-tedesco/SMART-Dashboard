# README ----------------------------------------------------
# ===========================================================

## data.R
## first treatment assignment for clinical trial dashboard

## init NT 04/05/2023
## updated NT 04/05/2023

## goals:

  # 1) Create treatment assignment data for 62 patients
  # 2) Account for stages 1 and 2



# packages --------------------------------------------------
# ===========================================================

library(dplyr)



# helper functions -------------------------------------------
# ============================================================

  ## treatment assignment
  ## --------------------

  stage1 <- function() {
    
    # README ----
    # purpose of function is to assign patient to first stage of treatment
    
    # treatment stage 1 options
    treatments <- c('IV Methylprednisolone 30mg BID', 'Upadacitinib 30mg BID', 'IV Methylprednisolone 30mg BID + Upadacitinib 45mg daily')
    
    # random assignment
    sample(treatments, 1)
    
  }
  
  stage2 <- function(treat1) {
    
    # README ----
    # purpose of function is to assign patient to second stage of treatment, given first treatment as input
    
    # treatment stage 2 options (dependent on first stage)
    treatments <- c()
    
    if(treat1 == 'IV Methylprednisolone 30mg BID') {
      treatments <- c('Add Cyclosporine Rescue', 'Add Upadacitinib 30mg BID Rescue')
    } else if (treat1 == 'Upadacitinib 30mg BID') {
      treatments <- c('Switch to IV Methylprednisolone 30mg BID + Cyclosporine', 'Add IV Methylprednisolone 30mg BID')
    } else {
      treatments <- c('Escalate to IV Methylprednisolone 30mg BID + Upadacitinib 30mg BID', 'Switch to Cyclosporine Rescue')
    }
    
    # random assignment
    sample(treatments, 1)
    
  }
  
  
  
  ## generating data
  ## ------------------
  
  toDF <- function(cohort_size) {
    
    # README ----
    # purpose of function is to generate treatment dataset (including stages 1 and 2) for given 
    # cohort size of patients
    
    data <- data.frame(patient_number = 1:cohort_size) %>%
      rowwise() %>%
      mutate(
        treatment_one = stage1(), 
        treatment_two = stage2(treatment_one)
      )
    
  }
  
  

# create and save data ---------------------------------------
# ============================================================

set.seed(25324)             # set seed for reproducibility
treatmentData <- toDF(62)

write.csv(treatmentData, 'treatmentData.csv')

  
  
  
