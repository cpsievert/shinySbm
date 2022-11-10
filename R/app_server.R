#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {



  datasetSelected <- eventReactive(input$mainDataSelector,isolate({
    if(input$whichData == 'importData'){
      input$mainDataFile$datapath
    }else{
      input$dataBase
    }
  }))

  sep <- reactive({
    if(input$whichSep == "others"){
      input$whichSep_other
    }else{
      input$whichSep
    }
  })

  datasetUploaded <- eventReactive(input$mainDataUploader,isolate({
    if(input$whichData == 'importData'){
      validate(
        need(datasetSelected() != "", "Please select a data set")
      )
      if(input$headerrow){
        x <- read.table(file = datasetSelected(), sep = sep(), row.names = 1, header = input$headercol)
      }else{
        x <- read.table(file = datasetSelected(), sep = sep(), header = input$headercol)
      }
      buildSbmMatrix(x)
    }else{
      validate(
        need(datasetSelected(), "Please select a data set")
      )
      switch(datasetSelected(),
             "fungus_tree" = buildSbmMatrix(sbm::fungusTreeNetwork$fungus_tree, col_names = sbm::fungusTreeNetwork$tree_names,
                                            row_names = sbm::fungusTreeNetwork$fungus_names),
             "tree_tree" = buildSbmMatrix(sbm::fungusTreeNetwork$tree_tree, col_names = sbm::fungusTreeNetwork$tree_names,
                                          row_names = sbm::fungusTreeNetwork$tree_names))

    }
  }))

  observeEvent(datasetUploaded(),{
    if(datasetUploaded()$type == "unipartite"){
      updateRadioButtons(session, "networkType", "What kind of network it is ?",
                         choices = list("Unipartite" = "unipartite","Bipartite" = "bipartite"),
                         inline = T)
    }else{
      updateRadioButtons(session, "networkType", "What kind of network it is ?",
                         choices = list("Bipartite" = "bipartite"),
                         inline = T)
    }
    updateSelectInput(session,"whichLaw",
                      label = "What is the density expected upon dataset ?",
                      choices = list("Bernoulli" = "bernoulli",
                                     "Poisson" = "poisson",
                                     "Gaussian" = "gaussian"),
                      selected = datasetUploaded()$law)
  })


  output$summaryDataImport <- renderPrint({
    print(datasetUploaded())
  })

  my_sbm <- eventReactive(input$runSbm,isolate({
    datasetup <- datasetUploaded()
    if(input$networkType == 'unipartite'){
      return(sbm::estimateSimpleSBM(netMat = datasetup$matrix, model = input$wichLaw))
    }else{
      return(sbm::estimateBipartiteSBM(netMat = datasetup$matrix, model = input$wichLaw))
    }
  }))

  output$sbmSummary <- renderPrint({
    dat <- my_sbm()
    print(dat$storedModels)
  })

  # observeEvent(my_sbm(),{
  # updateSliderInput(session,inputId = "Nbblocks2",
  #                   label = "Select the number of blocks:",
  #                   value = my_sbm()$nbBlocks[1], min = min(my_sbm()$storedModels$nbBlocks), max = max(my_sbm()$storedModels$nbBlocks),step=1)
  # })


  # microplot_base <- eventReactive(my_sbm(),isolate({
  #   my_sbm()$storedModels %>%  ggplot() + aes(x = nbBlocks, y = ICL)  + geom_line() + geom_point(alpha = 0.5)
  #   }))









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





  # microplot <- eventReactive(c(input$Nbblocks1,input$Nbblocks2,input$Nbblocks3),{
  #   return(plot(1:50+rnorm(50)))})
  #
  #
  # output$showILC1 <- renderPlot({microplot()})
  # output$showILC2 <- renderPlot({microplot()})
  # output$showILC3 <- renderPlot({microplot()})


  # shut down the app when it's closes on the browser
  session$onSessionEnded(function() {
    stopApp()
  })
}
