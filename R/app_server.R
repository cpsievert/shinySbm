#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  oldButton <- eventReactive(input$dataFile,{input$new})
  datasetread <- reactive({
    read.csv2(input$dataFile$datapath,row.names=1)
  })

  output$matrixPlot <- renderDataTable({
    datasetread()
  })
}
