# README ----------------------------------------------------
# ===========================================================

## app.R

## init NT 03/29/2023
## updated NT 04/05/2023

## goals:

  # 1) Reveal patient treatment assignment
  # 2) Track medication administration (test)
  # 3) Track symptoms, response status



# packages --------------------------------------------------
# ===========================================================

library(shiny)
library(dplyr)
library(googlesheets4)
library(shinythemes)
library(shinyTime)



# data ------------------------------------------------------
# ===========================================================

source('treatment_assignment.R')
source('functions.R')
  
  

# setup google sheets ---------------------------------------
# ===========================================================

  ## authentication
  ## --------------

  options(gargle_oauth_cache = ".secrets")
  gs4_auth()
  list.files(".secrets/")
  gs4_deauth()
  gs4_auth(cache = ".secrets", email = "tedesco1999@gmail.com")
  
  ## sheet IDs
  ## --------------

  treatment_sheet <- 'https://docs.google.com/spreadsheets/d/1tRd9zH0g1_igGho8kpiNyqFx0Z6b15S6sM6-HqrtPHg/edit#gid=0'

  

# define pages of application --------------------------------
# ============================================================

  ## Page 1: View Treatment Assignment
  ## ------------------------------------

  treatment_page <- tabPanel(
    # titles 
    title = 'Treatment',
    titlePanel(h1('Treatment Assignment', align = 'center')), 
    br(''), 
    # sidebar
    sidebarLayout(
      # randomization
      sidebarPanel(
        title = 'Inputs',
        textInput(
          inputId = 'patient_id', 
          label = 'Patient ID'
        ),
        checkboxInput(
          inputId = 'second_treatment', 
          label = 'Second Treatment?', 
        ), 
        actionButton(
          inputId = 'submit', 
          label = 'Show Assignment')
      ), 
      # show current data
      mainPanel(
        DT::dataTableOutput('responses'), 
        tags$a(href="https://docs.google.com/spreadsheets/d/1feaiNDE7cn_yOnrLjUdxa19aw7P_vX30RNGvwzvikPs/edit?pli=1#gid=1743807854", "View all assignments")
      )
    )
  )
  
  
  ## Page 2: Medication Tracker
  ## --------------------------
  
  med_page <- tabPanel(
    # titles
    title = 'Medications', 
    titlePanel('Medication Tracker'), 
    # sidebar
    sidebarLayout(
      sidebarPanel(
        title = 'Medication Inputs', 
        textInput(
          'med_patient_id',       # input id
          'Patient ID'            # label
        ), 
        selectInput(
          'med_treatment_stage',  # input id
          'Treatment Stage',      # label
          choices = c('Not Selected' = 0, 'First' = 1, 'Second' = 2)
        ), 
        dateInput(
          'med_date', 
          'Treatment Date'
        ),
        timeInput(
          'med_time', 
          'Treatment Time'
        ), 
        actionButton(
          'med_submit',           # input id
          'Submit'                # label
        )
      ), 
      mainPanel()
    )
  )

  ## Page 3: Dashboard Guide
  ## ------------------------

  about_page <- tabPanel(
    title = 'Guide',
    titlePanel('About'),
    'The purpose of this application is to log clinical trials data for the following feasibility trial: 
    A Pilot Sequential Multiple Assignment Randomized Trial (SMART) Developing 
    and Optimizing Patient-Tailored Adaptive Treatment Strategies (ATS) for Acute Severe Ulcerative Colitis'
  )


# define ui --------------------------------------------------
# ============================================================

ui <- navbarPage('Clinical Trial Dashboard',
  treatment_page,
  med_page, 
  about_page,
  theme = shinytheme(theme = 'spacelab')
)


# define server ----------------------------------------------
# ============================================================

server <- function(input, output, session) {
  
  ## treatment reveal (tab 1)
  ## ------------------
  
    # input
    id <- eventReactive(input$submit, input$patient_id)
    treat2 <- eventReactive(input$submit, input$second_treatment)
    
    # retrieve row of interest
    getRow <- reactive({
      reveal_row(id(), treat2())
    })
  
    # save data upon submit click
    observeEvent(input$submit, {
      saveData(getRow(), treatment_sheet)
    })
    
    # Show the previous responses, updated with current
    output$responses <- DT::renderDataTable({
      input$submit
      DT::datatable(orgData(loadData(treatment_sheet)), caption = 'Assignments revealed to-date')
    })
  
    
  ## medications (tab 2)
  ## -------------------
    
}

    
# run application --------------------------------------------
# ============================================================

shinyApp(ui = ui, server = server)
  
# references -------------------------------------------------
# ============================================================
  
  # https://mgolafshar.github.io/clinical-dashboards/
  # https://jenthompson.me/2018/02/09/flexdashboards-monitoring/
