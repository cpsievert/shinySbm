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
  output$matrixprint <- DT::renderDataTable({
    DT::datatable(
      as.data.frame(datasetInput()),
      option = list(
        # scroll :
        scrollY = 500, scrollX = 500, scroller = TRUE,
        lengthMenu = list(c(-1 ,50, 100),
                          c('All', '50', '100')),
        paging = T
      )
    )
  })

  window_height <- reactive({htmlwidgets::JS('window.innerHeight')})
  window_width <- reactive({htmlwidgets::JS('window.innerWidth')})

  output$matrixPlot <- renderPlot({
    x <- datasetInput()$matrix
    sbm::plotMyMatrix(x, dimLabels = list(row = input$rowLabel, col = input$colLabel))
  },height = 600 ,width = 600)
}
