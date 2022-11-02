#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  datasetread <- reactive({
    read.csv2(input$dataFile$datapath,row.names=1)
  })

  output$matrixPlot <- renderDataTable({
    datasetread()
  })
}
