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
    # probleme : taille et position, wrapping des titres, fixer la colonnne de rownames
    req(input$whichshow)
    if(input$whichshow != 'print'){return(NULL)}
    DT::datatable(
      as.data.frame(datasetInput()),
      class = 'nowrap',
      option = list(
        # scroll :
        scrollY = 700, scrollX = 700, scroller = TRUE,
        lengthMenu = list(c(-1 ,50, 100),
                          c('All', '50', '100')),
        paging = T
      )
    )
  })

  output$matrixplot <- renderPlot({
    # probleme : taille et position,
    req(input$whichshow)
    if(input$whichshow != 'simpleplot'){return(NULL)}
    x <- datasetInput()$matrix
    sbm::plotMyMatrix(x, dimLabels = list(row = input$rowLabel, col = input$colLabel))
  }#,height = 600 ,width = 600
  )

  output$matrixplot2 <- renderPlot({
  # probleme : taille et position, introduce les noms de colonnes etc ...
    # voir si la fonction plotMat est adaptable pour conserver les noms de colonnes
    # (meme une fois la matrice organisee)
    req(input$whichshow)
    if(input$whichshow != 'namedplot'){return(NULL)}
    x <- datasetInput()$matrix
    blockmodeling::plotMat(x)
  }#,height = 600 ,width = 600
  )
}
