#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {


  datasetInput <- reactive({
    req(input$whichData)
    if(input$whichData == 'importData'){
      req(input$dataFile$datapath)
      buildSbmMatrix(read.csv2(input$dataFile$datapath,row.names=1))
    }else{
      req(input$dataBase)
      switch(input$dataBase,
             "fungus_tree" = buildSbmMatrix(sbm::fungusTreeNetwork$fungus_tree, col_names = sbm::fungusTreeNetwork$tree_names,
                                            row_names = sbm::fungusTreeNetwork$fungus_names),
             "tree_tree" = buildSbmMatrix(sbm::fungusTreeNetwork$tree_tree, col_names = sbm::fungusTreeNetwork$tree_names,
                                          row_names = sbm::fungusTreeNetwork$tree_names))
    }
  })
  interactiveTable <- eventReactive(output$matrixPlot,{T})
  output$matrixPlot <- renderPlot({
    x <- datasetInput()$matrix
    sbm::plotMyMatrix(x, dimLabels = list(row = input$rowLabel, col = input$colLabel))
  })
}
