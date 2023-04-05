# README ----------------------------------------------------
# ===========================================================

## functions.R

## init NT 04/05/2023
## updated NT 04/05/2023

## goals:

  # 1) Create helper functions for dashboard to reference



# packages ---------------------------------------------------
# ============================================================

library(dplyr)



# helper functions -------------------------------------------
# ============================================================

  ## reveal patient treatments
  ## -------------------------

  reveal_row <- function(id, treat2) {
    
    # README ----
    # purpose: reveals row for given patient
    # inputs: patient id, T/F for reveal second treatment
    
    if(treat2) {
      row <- data.frame(treatmentData %>% filter(patient_id == id))
    } else {
      row <- data.frame(treatmentData %>% filter(patient_id == id))
      # set second treatment to 'hidden' to hide contents
      row$treatment_two <- 'hidden'   
    }
    
    # get time to sort by most recent reveal
    row$assignment_time <- Sys.time()
    
    return(row)
    
  }

  overwrite_row <- function() {}

  ## google sheets, save and load data
  ## ------------------

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
  
  ## organize treatment data
  ## -----------------------
  
  orgData <- function(data) {
    
    # README ----
    # purpose: retrieve most recent treatment row for each patient id, sort results by date
    # inputs: treatment data (from loadData())
    
    data <- data %>% 
      group_by(patient_id) %>% 
      slice(which.max(assignment_time)) %>%
      ungroup() %>%
      arrange(desc(assignment_time)) %>%
      select(-assignment_time)
    
    return(data)
    
  }
  
  