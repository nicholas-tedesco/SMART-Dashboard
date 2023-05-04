# README -----------------------------------------------------
# ============================================================

# assignment-tab.R.R

# init NT 04/11/2023
# updated NT 05/01/2023

# purpose: create standalone application that reveals 
#          patient treatment assignment



# setup ------------------------------------------------------
# ============================================================

  ## packages, data, and functions ----

  library(shiny)
  library(dplyr)
  library(googlesheets4)
  library(shinythemes)
  library(shinyTime)
  library(DT)
  library(data.table)
  
  source('functions.R')
  source('redcap-data.R')
  
  ## google sheets ----
  
  options(gargle_oauth_cache = ".secrets")
  gs4_auth()
  list.files(".secrets/")
  gs4_deauth()
  gs4_auth(cache = ".secrets", email = "tedesco1999@gmail.com")
  
  treatment_sheet <- 'https://docs.google.com/spreadsheets/d/1tRd9zH0g1_igGho8kpiNyqFx0Z6b15S6sM6-HqrtPHg/edit#gid=0'



# ui ---------------------------------------------------------
# ============================================================
    
assign_ui <- fluidPage(
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
      dataTableOutput('responses')
    )
  )
)



# server -----------------------------------------------------
# ============================================================

assign_server <- function(input, output, session) {
  
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
  output$responses <- renderDataTable(
    {input$submit
    datatable(
      treat_toDF(loadData(treatment_sheet)), 
      colnames = c('ID', 'Stage 1', 'Stage 2'), 
      rownames = FALSE, 
      options = list(
        paging = FALSE, 
        dom = 't', 
        scrollY = 200
      )
    )}
  )

}



# references -------------------------------------------------
# ============================================================

# https://mgolafshar.github.io/clinical-dashboards/
# https://jenthompson.me/2018/02/09/flexdashboards-monitoring/
