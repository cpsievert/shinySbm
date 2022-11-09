#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {


  datasetSelected <- reactive({
    req(input$whichData)
    if(input$whichData == 'importData'){
      req(input$mainDataFile$datapath)
      buildSbmMatrix(read.csv2(input$mainDataFile$datapath,row.names=1))
    }else{
      req(input$dataBase)
      switch(input$dataBase,
             "fungus_tree" = buildSbmMatrix(sbm::fungusTreeNetwork$fungus_tree, col_names = sbm::fungusTreeNetwork$tree_names,
                                            row_names = sbm::fungusTreeNetwork$fungus_names),
             "tree_tree" = buildSbmMatrix(sbm::fungusTreeNetwork$tree_tree, col_names = sbm::fungusTreeNetwork$tree_names,
                                          row_names = sbm::fungusTreeNetwork$tree_names))
      }
    })



  datasetUploaded <- reactive({datasetSelected()})

  output$summaryDataImport <- renderPrint({
    print(datasetSelected())
  })


  output$matrixPrint <- DT::renderDataTable({
    # probleme : taille et position, wrapping des titres, fixer la colonnne de rownames
    req(input$whichShow)
    if(input$whichShow != 'print'){return(NULL)}
    DT::datatable(
      as.data.frame(datasetUploaded()),
      option = list(
        # scroll :
        scroller = TRUE,
        lengthMenu = list(c(-1 ,50, 100),
                          c('All', '50', '100')),
        paging = T))
    })

  Plot <- reactive({
    req(input$whichShow)
    if(input$whichShow == 'plot'){
      x <- as.matrix(datasetUploaded())
      sbm::plotMyMatrix(x, dimLabels = list(row = input$rowLabel, col = input$colLabel))
    }else{
      return(NULL)
    }
  })

  output$matrixPlot <- renderImage({
    # Read myImage's width and height. These are reactive values, so this
    # expression will re-run whenever they change.
    width  <- session$clientData$output_matrixPlot_width
    height <- session$clientData$output_matrixPlot_width

    # For high-res displays, this will be greater than 1
    pixelratio <- session$clientData$pixelratio

    # A temp file to save the output.
    outfile <- tempfile(fileext='.png')

    # Generate the image file
    png(outfile, width = width*pixelratio, height = height*pixelratio,
        res = 72*pixelratio)
    plot(Plot())
    dev.off()

    # Return a list containing the filename
    list(src = outfile,
         width = width,
         height = height,
         alt = "This is alternate text")
  }, deleteFile = TRUE)



  microplot <- eventReactive(c(input$NbGroup1,input$NbGroup2,input$NbGroup3),{return(plot(1:50+rnorm(50)))})


  output$showILC1 <- renderPlot({microplot()})
  output$showILC2 <- renderPlot({microplot()})
  output$showILC3 <- renderPlot({microplot()})


  # shut down the app when it's closes on the browser
  session$onSessionEnded(function() {
    stopApp()
  })
}
