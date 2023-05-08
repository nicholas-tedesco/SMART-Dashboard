# README -----------------------------------------------------------------
# ========================================================================

# functions.R
# purpose: create functions for dashboard and helper files

# init NT 04/05/2023
# updated NT 04/12/2023

# notes

  # working on functions for REDCap data pull
  # functions for home page? do we even need home page? (guide at the very least)



# packages ---------------------------------------------------------------
# ========================================================================

library(dplyr)
library(httr)
library(tidyverse)
library(lubridate)



# google sheets ----------------------------------------------
# ------------------------------------------------------------

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



# REDCap -----------------------------------------------------------------
# ========================================================================

  ## retrieve data from REDCap ---------------------------------
  ## -----------------------------------------------------------

  get_rc <- function(form_id) {
    
    # README ----
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
  


# treatment assignment ---------------------------------------
# ------------------------------------------------------------
  
  ## assignment ----
  
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

  ## reveal patient treatments ----

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
    
    return(row)
    
  }
  
  ## organize treatment data ----
  
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
  
  assign_patients <- function(cohort_size) {
    
    # README ----
    # purpose of function is to generate treatment dataset (including stages 1 and 2) for given 
    # cohort size of patients
    
    data <- data.frame(patient_id = 1:cohort_size) %>%
      rowwise() %>%
      mutate(
        treatment_one = stage1(), 
        treatment_two = stage2(treatment_one)
      )
    
  }
  
  

# assessment -------------------------------------------------
# ------------------------------------------------------------
  
  ## organize assessment data ----
  
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
  

# symptoms ---------------------------------------------------
# ------------------------------------------------------------
  
  ## generate symptom plot ----
  ## --------------------------
  
  symptom_plot1 <- function(id){
    
    test_snr %>%
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
  }
  
  symptom_plot2 <- function(id){
    test_snr %>%
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
  }
  
# ggPieDonut change colors ----
  
  custom_ggPieDonut <- function (data, mapping, addPieLabel = TRUE, addDonutLabel = TRUE, 
                                 showRatioDonut = TRUE, showRatioPie = TRUE, showRatioPieAbove10 = TRUE, 
                                 title = "", labelposition = 1, polar = TRUE, use.label = TRUE, 
                                 use.labels = TRUE, interactive = FALSE, palette = 'Set 1') 
  {
    (cols = colnames(data))
    if (use.labels) 
      data = addLabelDf(data, mapping)
    count <- NULL
    if ("y" %in% names(mapping)) {
      count <- getMapping(mapping, "y")
    }
    else {
      if ("count" %in% names(mapping)) 
        count <- getMapping(mapping, "count")
    }
    count
    (pies = getMapping(mapping, "pies"))
    (donuts = getMapping(mapping, "donuts"))
    if ((length(pies) + length(donuts)) != 2) {
      (xvar = getMapping(mapping, "x"))
      if (length(xvar) > 1) {
        pies = xvar[1]
        donuts = xvar[2]
      }
    }
    if ((length(pies) + length(donuts)) == 1) {
      if (is.null(pies)) {
        p <- ggDonut(data, mapping, addDonutLabel = addDonutLabel, 
                     showRatio = showRatioDonut, title = title, labelposition = labelposition, 
                     polar = polar, interactive = interactive)
      }
      else {
        p <- ggPie(data, mapping, title = title, addPieLabel = addPieLabel, 
                   showRatioPie = showRatioPie, showRatioPieAbove10 = showRatioPieAbove10, 
                   labelposition = labelposition, polar = polar, 
                   interactive = interactive)
      }
    }
    else {
      if (is.null(count)) {
        dat1 = plyr::ddply(data, c(pies, donuts), nrow)
        colnames(dat1)[3] = "n"
        dat1$ymax = cumsum(dat1$n)
        dat1$ymin = cumsum(dat1$n) - dat1$n
        dat1$ypos = dat1$ymin + dat1$n/2
        dat1$ratio = dat1$n * 100/sum(dat1$n)
        dat1$cumratio = dat1$ypos * 100/sum(dat1$n)
        dat1$hjust = ifelse((dat1$cumratio > 25 & dat1$cumratio < 
                               75), 0, 1)
        dat1$label = paste0(dat1[[pies]], "<br>", dat1[[donuts]], 
                            "<br>", dat1$n, "(", round(dat1$ratio, 1), "%)")
        data2 = plyr::ddply(data, pies, nrow)
        colnames(data2)[2] = "sum"
        data2$cumsum = cumsum(data2$sum)
        data2$pos = data2$cumsum - data2$sum/2
        data2$ymin = data2$cumsum - data2$sum
        data2$ratio = data2$sum * 100/sum(data2$sum)
        data2$label = ifelse(data2$ratio > 10, paste0(data2[[pies]], 
                                                      "<br>", data2$sum, "(", round(data2$ratio, 1), 
                                                      "%)"), paste0(data2[[pies]]))
        data2$tooltip = paste0(data2[[pies]], "<br>", data2$sum, 
                               "(", round(data2$ratio, 1), "%)")
      }
      else {
        dat1 = data
        colnames(dat1)[colnames(dat1) == count] = "n"
        dat1$ymax = cumsum(dat1$n)
        dat1$ymin = cumsum(dat1$n) - dat1$n
        dat1$ypos = dat1$ymin + dat1$n/2
        dat1$ratio = dat1$n * 100/sum(dat1$n)
        dat1$cumratio = dat1$ypos * 100/sum(dat1$n)
        dat1$hjust = ifelse((dat1$cumratio > 25 & dat1$cumratio < 
                               75), 0, 1)
        dat1$label = paste0(dat1[[pies]], "<br>", dat1[[donuts]], 
                            "<br>", dat1$n, "(", round(dat1$ratio, 1), "%)")
        dat1
        pies
        data2 = eval(parse(text = "plyr::ddply(dat1,pies,summarize,sum(n))"))
        data2
        colnames(data2)[2] = "sum"
        data2 = data2[order(data2$sum, decreasing = TRUE), 
        ]
        data2$cumsum = cumsum(data2$sum)
        data2$pos = data2$cumsum - data2$sum/2
        data2$ymin = data2$cumsum - data2$sum
        data2$ratio = data2$sum * 100/sum(data2$sum)
        data2$label = ifelse(data2$ratio > 10, paste0(data2[[pies]], 
                                                      "<br>", data2$sum, "(", round(data2$ratio, 1), 
                                                      "%)"), paste0(data2[[pies]]))
        data2$tooltip = paste0(data2[[pies]], "<br>", data2$sum, 
                               "(", round(data2$ratio, 1), "%)")
      }
      mainCol = palette.colors(n=nrow(data2), palette=palette)
      subCol = subcolors(dat1, pies, mainCol)
      p <- ggplot(dat1) + geom_rect_interactive(aes_string(ymax = "ymax", 
                                                           ymin = "ymin", xmax = "4", xmin = "3", tooltip = "label", 
                                                           data_id = donuts), fill = subCol, colour = "white")
      p <- p + geom_rect_interactive(aes_string(ymax = "cumsum", 
                                                ymin = "ymin", xmax = "3", xmin = "0", tooltip = "tooltip", 
                                                data_id = pies), data = data2, fill = mainCol, colour = "white", 
                                     alpha = 0.7)
      p <- p + theme_void()
      if (addDonutLabel) {
        label2 = dat1[[donuts]]
        if (showRatioDonut) 
          label2 = paste0(label2, "\n(", round(dat1$ratio, 
                                               1), "%)")
        if (polar) {
          if (labelposition == 1) {
            p <- p + geom_text(aes_string(label = "label2", 
                                          x = "4.3", y = "ypos", hjust = "hjust"), 
                               size = 3) + geom_segment(aes_string(x = "4", 
                                                                   xend = "4.2", y = "ypos", yend = "ypos"))
          }
          else {
            p <- p + geom_text(aes_string(label = "label2", 
                                          x = "3.5", y = "ypos"), size = 3)
          }
        }
        else {
          p <- p + geom_text(aes_string(label = "label2", 
                                        x = "3.5", y = "ypos"), size = 3)
        }
      }
      if (addPieLabel) {
        Pielabel = data2[[pies]]
        if (showRatioPie) {
          if (showRatioPieAbove10) {
            Pielabel = ifelse(data2$ratio > 10, paste0(data2[[pies]], 
                                                       "\n(", round(data2$ratio, 1), "%)"), paste0(data2[[pies]]))
          }
          else Pielabel = paste0(Pielabel, "\n(", round(data2$ratio, 
                                                        1), "%)")
        }
        p <- p + geom_text(data = data2, aes_string(label = "Pielabel", 
                                                    x = "1.5", y = "pos"), size = 4)
      }
      if (polar) 
        p <- p + coord_polar(theta = "y", start = 3 * pi/2)
      if (title != "") 
        p <- p + ggtitle(title)
      if (use.label) {
        labels = c()
        for (i in 1:length(cols)) {
          temp = get_label(data[[cols[i]]])
          labels = c(labels, ifelse(is.null(temp), cols[i], 
                                    temp))
        }
        labels
        p <- p + scale_x_discrete(labels = labels)
      }
      if (interactive) {
        tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;"
        p <- girafe(ggobj = p)
        p <- girafe_options(p, opts_tooltip(css = tooltip_css, 
                                            opacity = 0.75), opts_zoom(min = 1, max = 10))
      }
      p
    }
    p
  }