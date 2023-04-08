# README ----------------------------------------------------
# ===========================================================

## app.R

## init NT 03/29/2023
## updated NT 04/05/2023

## goals: create application that...

<<<<<<< HEAD
  # 1) Reveals patient treatment assignment
  # 2) Calculates treatment assessment times
  # 3) Visualizes assessment data
  # 4) Tracks patient enrollment
  # 5) Pulls data from REDCap
=======
  # 1) Reveal patient treatment assignment
  # 2) Track medication administration (test)
  # 3) Track symptoms, response status
>>>>>>> 6cb7e7e3f2413d144e3f53f3db5e49d90cee5529



# setup -----------------------------------------------------
# ===========================================================

  ## packages ----
  ## -------------

  library(shiny)
  library(dplyr)
  library(googlesheets4)
  library(shinythemes)
  library(shinyTime)
  library(DT)
  library(data.table)

  ## data ----
  ## ---------

  source('treatment_assignment.R')
  source('functions.R')
  
  ## google sheets 
  ## -------------

  options(gargle_oauth_cache = ".secrets")
  gs4_auth()
  list.files(".secrets/")
  gs4_deauth()
  gs4_auth(cache = ".secrets", email = "tedesco1999@gmail.com")

  treatment_sheet <- 'https://docs.google.com/spreadsheets/d/1tRd9zH0g1_igGho8kpiNyqFx0Z6b15S6sM6-HqrtPHg/edit#gid=0'

# pages of application ---------------------------------------
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
        dataTableOutput('responses'), 
        tags$a(href="https://docs.google.com/spreadsheets/d/1feaiNDE7cn_yOnrLjUdxa19aw7P_vX30RNGvwzvikPs/edit?pli=1#gid=1743807854", "View all assignments")
      )
    )
  )
  
  
  ## Page 2: Assessment Tracker
  ## --------------------------
  
  assess_page <- tabPanel(
    # titles
    title = 'Assessment', 
    titlePanel(h1('Assessment Times', align = 'center')), 
    br(''), 
    # output
    dataTableOutput('assessment')
  )


# define ui --------------------------------------------------
# ============================================================

ui <- navbarPage('SMART Dashboard', 
  treatment_page,
  assess_page, 
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
    output$responses <- renderDataTable({
      input$submit
      datatable(
        treat_toDF(loadData(treatment_sheet)), 
        colnames = c('ID', 'Treatment 1', 'Treatment 2'), 
        rownames = FALSE, 
        caption = 'Assignments revealed to-date'
      )
    })
  
    
  ## assessment tracker (tab 2)
  ## -------------------
    
    # create output dataset
    output$assessment <- renderDataTable(
      
      datatable(
        assess_toDF(assessData), 
        rownames = FALSE, 
        colnames = c('ID', 'Treatment Start Time', '24 Hours', '48 Hours', '72 Hours'), 
        options = list(
          dom = 'ft', 
          columnDefs = list(list(visible=FALSE, targets=c(5:7))), 
          pageLength = 1000
        )
        
      ) %>% 
        formatDate(~start_time + day_one + day_two + day_three, method = 'toLocaleString') %>%
        formatStyle(
          'day_one', 
          'time_until_day_one', 
          backgroundColor = styleInterval(c(-6, 6), c('white', 'red', 'white'))
        ) %>% 
        formatStyle(
          'day_two',
          'time_until_day_two', 
          backgroundColor = styleInterval(c(-6, 6), c('white', 'red', 'white'))
        ) %>%
        formatStyle(
          'day_three', 
          'time_until_day_three', 
          backgroundColor = styleInterval(c(-6, 6), c('white', 'red', 'white'))
        )
    )
    
}

    
# run application --------------------------------------------
# ============================================================

shinyApp(ui = ui, server = server)
  
# references -------------------------------------------------
# ============================================================
  
  # https://mgolafshar.github.io/clinical-dashboards/
  # https://jenthompson.me/2018/02/09/flexdashboards-monitoring/
