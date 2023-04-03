## README ----

# app.R
# clinical trial dashboard

# init NT 03/29
# updated NT 03/30

# application goals:

  # 1) Randomize patient to initial treatment, and log results
  # 2) ...
  # 3) ...


## packages ----

library(shiny)
library(dplyr)
library(shinythemes)


## helper functions ----

  # treatment assignment

  randomizer <- function(num, treat1){
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

  random_to_df <- function(id, num, assign){
    data.frame(
      'patient_id' = id, 
      'treatment_number' = num, 
      'assignment' = assign, 
      'assignment_time' = Sys.time()
    )
  }

  # saving and loading data

  outputDir <- "C:/Users/nick_/Documents/Work/U-M/Berinstein/responses"

  saveData <- function(data) {
    # Create a unique file name
    fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
    # Write the file to the local system
    write.csv(
      x = data,
      file = file.path(outputDir, fileName), 
      row.names = FALSE, quote = TRUE
    )
  }

  loadData <- function() {
    # Read all the files into a list
    files <- list.files(outputDir, full.names = TRUE)
    if(length(files) == 0) {
      return(NULL)
    } else {
      data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
      # Concatenate all data together into one data.frame
      data <- data.frame(do.call(rbind, data)) %>% arrange(desc(assignment_time))
      return(data)
    }
  }


## define pages of application ----

  # Page 1: Treatment Assignment and Log

  AA_page <- tabPanel(
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
        DT::dataTableOutput('responses')
      # error message
      
      )
    )
  )


  ## About Page

  about_page <- tabPanel(
    title = 'Guide',
    titlePanel('About'),
    'The purpose of this application is to log clinical trials data for the following feasibility trial: 
    A Pilot Sequential Multiple Assignment Randomized Trial (SMART) Developing 
    and Optimizing Patient-Tailored Adaptive Treatment Strategies (ATS) for Acute Severe Ulcerative Colitis'
  )


## define ui ----

ui <- navbarPage('Clinical Trial Dashboard',
  AA_page,
  about_page,
  theme = shinytheme(theme = 'flatly')
)


## define server ----

server <- function(input, output, session) {
  
  # randomizer (tab 1)
  
    # input from randomizer
    id <- eventReactive(input$submit, input$patient_id)
    num <- eventReactive(input$submit, input$treatment_num)
    
    # generate data
    formData <- reactive({
      random_to_df(id(), num(), randomizer(num(), treat1()))
    })
  
    # save data upon submit click
    observeEvent(input$submit, {
      saveData(formData())
    })
    
    # Show the previous responses, updated with current
    output$responses <- DT::renderDataTable({
      input$submit
      loadData()
    })
    
    # keep track of treatment 1 for given patient 
    treat1 <- eventReactive(
      input$submit, 
      data.frame(loadData()) %>% filter(patient_id == id()) %>% select(assignment)
    )
 
}

    
## run application ----

shinyApp(ui = ui, server = server)
