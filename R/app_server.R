#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {


  datasetInput <- reactive({
    if(input$whichData == 'importData'){
      buildSbmMatrix(read.csv2(input$dataFile$datapath,row.names=1))
    }
    switch(input$dataBase,
           "fungus_tree" = buildSbmMatrix(sbm::fungusTreeNetwork$fungus_tree, col_names = sbm::fungusTreeNetwork$tree_names,
                                          row_names = sbm::fungusTreeNetwork$fungus_names),
           "tree_tree" = buildSbmMatrix(sbm::fungusTreeNetwork$tree_tree, col_names = sbm::fungusTreeNetwork$tree_names,
                                        row_names = sbm::fungusTreeNetwork$tree_names)
    )
  })

  output$matrixPlot <- renderDataTable({
    data_show <- as.data.frame(datasetInput())
    cbind(NODES_NAMES = rownames(data_show), data_show)
  })
}
