symptom_ui <- fluidPage(
  br(''), 
  # sidebar
  sidebarLayout(
    sidebarPanel(
      title = 'Inputs',
      textInput(
        inputId = 'patient_id', 
        label = 'Patient ID'
      ),
      actionButton(
        inputId = 'refresh', 
        label = 'Refresh'), 
      width = 2
    ), 
    mainPanel(
      plotlyOutput(
        outputId = 'symptom_plot', 
        height = '175px', width = '100%'
      ), 
      width = 10
    )
  )
)

symptom_server <- function(input, output, session) {
  
  # input
  id <- eventReactive(input$refresh, input$patient_id)
  
  # generate plot
  getPlot <- reactive({
    symptom_plot(id())
  })
  
  # save plot to output
  output$symptom_plot <- renderPlotly({
    input$refresh
    getPlot()
  })
  
}

shinyApp(
  ui = symptom_ui, 
  server = symptom_server
)