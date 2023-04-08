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



# home page --------------------------------------------------
# ============================================================


tab_voronoys <- function(texto, cor){
  HTML(paste0('<a href="#" class="action-button">
                  <div class = "voronoys-block" style = "background-color:', cor, ';"> 
                  <span class = "name">', texto, '</span>
                  <div class="img_block">
                    <div class="img_block_conteiner">
                    </div>
                  </div>
              </div></a>'))
}


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
      row$treatment_two <- '[hidden]'   
    }
    
    # get time to sort by most recent reveal
    row$assignment_time <- Sys.time()
    
    return(row)
    
  }

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
  
  treat_toDF <- function(data) {
    
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
  
  ## organize assessment data
  ## ------------------------
  
  assess_toDF <- function(assessData) {
    
    # README ----
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
  
  