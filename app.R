# README ----------------------------------------------------
# ===========================================================

## app.R
## clinical trial dashboard

## init NT 03/29
## updated NT 04/03

## application goals:

  # 1) Randomize patient to initial treatment, and log results
  # 2) Track medication administration
  # 3) Track symptoms, response status



# packages --------------------------------------------------
# ===========================================================

library(shiny)
library(dplyr)
library(shinythemes)
library(googlesheets4)



# google sheets ---------------------------------------------
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
  medication_sheet <- ''

  
  
# helper functions -------------------------------------------
# ============================================================

  ## treatment assignment
  ## --------------------

  randomizer <- function(num, treat1){
    
    # README ----
    
    # Purpose of function is to assign given patient to first or second stage of treatment. 
    # Second stage of treatment depends on first stage. 
    
    # first treatment
    if(num == 1) {
      choices1 <- c('IV methylprednisolone (30mg twice daily)', 'upadacitinib (30mg twice daily)', 'IV methylprednisolone (30mg twice daily) + upadacitinib (45mg daily)')
      sample(choices1, 1)
    # second treatment (depends on first treatment)
    } else {
      if(treat1 == 'IV methylprednisolone (30mg twice daily)') {
        choices2 <- c('add oyolosporin rescue', 'add upa 30mg BID rescue')
        sample(choices2, 1)
      } else if (treat1 == 'upadacitinib (30mg twice daily)') {
        choices2 <- c('switch to IV methylprednisolone 30 mg BID + cycloxporine', 'add IV methylprednisolone 30mg BID')
        sample(choices2, 1)
      } else {
        choices2 <- c('increase upa to 30mg BID', 'switch to oyolosporin rescue')
        sample(choices2, 1)
      }
    }
  }
  
  ## collect results of randomization
  ## --------------------------------

  random_to_df <- function(id, num, assign){
    data.frame(
      'patient_id' = id, 
      'treatment_number' = num, 
      'assignment' = assign, 
      'assignment_time' = Sys.time()
    )
  }

  ## save and load data
  ## ------------------

  saveData <- function(data, SHEET_ID) {
    # The data must be a dataframe rather than a named vector
    data <- data %>% as.list() %>% data.frame()
    # Add the data as a new row
    sheet_append(SHEET_ID, data)
  }

  loadData <- function(SHEET_ID) {
    # Read the data
    data <- data.frame(read_sheet(SHEET_ID))
    data %>% arrange(desc(assignment_time))
}


# define pages of application --------------------------------
# ============================================================

  ## Page 1: Treatment Assignment and Log
  ## ------------------------------------

  treat_page <- tabPanel(
    # titles 
    title = 'Randomizer',
    titlePanel('Treatment Assignment'),
    # sidebar
    sidebarLayout(
      # randomizer
      sidebarPanel(
        title = 'Inputs',
        textInput(inputId = 'patient_id', label = 'Patient ID'),
        selectInput('treatment_num', 'Treatment Number', choices = c('Not Selected' = 0, 'First' = 1, 'Second' = 2)),
        actionButton('submit', 'Submit')),
      # show current data
      mainPanel(
        DT::dataTableOutput('responses'), 
        tags$a(href="https://docs.google.com/spreadsheets/d/1tRd9zH0g1_igGho8kpiNyqFx0Z6b15S6sM6-HqrtPHg/edit#gid=0", "Access data")
      )
    )
  )
  
  
  ## Page 2: Medication Tracker
  ## --------------------------


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
  treat_page,
  about_page,
  theme = shinytheme(theme = 'flatly')
)


# define server ----------------------------------------------
# ============================================================

server <- function(input, output, session) {
  
  ## randomizer (tab 1)
  ## ------------------
  
    # input from randomizer
    id <- eventReactive(input$submit, input$patient_id)
    num <- eventReactive(input$submit, input$treatment_num)
    
    # keep track of treatment 1 for given patient 
    treat1 <- eventReactive(
      input$submit, 
      data.frame(loadData(treatment_sheet)) %>% filter(patient_id == id()) %>% select(assignment)
    )
    
    # generate data
    formData <- reactive({
      random_to_df(id(), num(), randomizer(num(), treat1()))
    })
  
    # save data upon submit click
    observeEvent(input$submit, {
      saveData(formData(), treatment_sheet)
    })
    
    # Show the previous responses, updated with current
    output$responses <- DT::renderDataTable({
      input$submit
      loadData(treatment_sheet)
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
