#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {


  datasetread <- reactive({buildSbmMatrix(read.csv2(input$dataFile$datapath,row.names=1))
  })

  datasetInput <- reactive({
    if (input$whichData != "importData") {
      data("fungusTreeNetwork")
    }
    switch(input$whichData,
           "fungus_tree" = buildSbmMatrix(fungusTreeNetwork$fungus_tree),
           "tree_tree" = buildSbmMatrix(fungusTreeNetwork$tree_tree),
           "importData" = datasetread()
    )
  })

  output$matrixPlot <- renderDataTable({
    datasetInput()$matrix
  })
}
