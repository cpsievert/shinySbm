#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  labels <- eventReactive(c(input$networkType,input$rowLabel,input$colLabel,input$nodLabel),{
    switch(input$networkType,
           'bipartite' = list(row = input$rowLabel, col = input$colLabel),
           'unipartite' = list(row = input$nodLabel, col = input$nodLabel))
  })




  datasetSelected <- eventReactive(c(input$whichData,input$dataBase,input$mainDataFile$datapath),{
    if(input$whichData == 'importData'){
      input$mainDataFile$datapath
    }else{
      input$dataBase
    }
  })

  sep <- reactive({
    if(input$whichSep == "others"){
      input$whichSep_other
    }else{
      input$whichSep
    }
  })

  datasetUploaded <- eventReactive(input$mainDataUploader,{
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
  })

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

  PlotMat <- reactive({
    req(input$whichShow)
    if(input$whichShow == 'plot'){
      x <- as.matrix(datasetUploaded())
      if(input$runSbm){
        data_sbm <- my_sbm()$clone()
        switch(input$whichRawSbmMatrix,
               "raw" = sbm::plotMyMatrix(x, dimLabels = labels()),
               "ordered" = plot(data_sbm, type = "data", dimLabels = labels()),
               "simple" = plot(data_sbm, type = "expected", dimLabels = labels()))
      }else{
        sbm::plotMyMatrix(x, dimLabels = labels())
      }
    }else{
      return(NULL)
    }
  })

  output$matrixPlot <- renderImage({
    # Read myImage's width and height. These are reactive values, so this
    # expression will re-run whenever they change.
    width  <- session$clientData$output_matrixPlot_width
    height <- session$clientData$output_matrixPlot_height

    # For high-res displays, this will be greater than 1
    pixelratio <- session$clientData$pixelratio

    # A temp file to save the output.
    outfile <- tempfile(fileext='.png')

    # Generate the image file
    png(outfile, width = width*pixelratio, height = height*pixelratio,
        res = 72*pixelratio)
    plot(PlotMat())
    dev.off()

    # Return a list containing the filename
    list(src = outfile,
         width = width,
         height = height,
         alt = "This is alternate text")
  }, deleteFile = TRUE)

  ### Problem : with bipartite two graphs ans two input
  my_sbm_main <- eventReactive(input$runSbm,{
    datasetup <- datasetUploaded()
    data_res <- withProgress(message = "SBM is Running", {
      switch (input$networkType,
              "unipartite" = sbm::estimateSimpleSBM(netMat = as.matrix(datasetup), model = input$whichLaw, estimOptions = list(verbosity = 0, plot = F)),
              "bipartite" = sbm::estimateBipartiteSBM(netMat = as.matrix(datasetup), model = input$whichLaw, estimOptions = list(verbosity = 0, plot = F)))
    })
    return(data_res)
    })

  observeEvent(input$runSbm,{
    data_sbm <- my_sbm_main()$clone()
    value <- data_sbm$nbBlocks
    min <- min(data_sbm$storedModels$nbBlocks)
    max <- max(data_sbm$storedModels$nbBlocks)
    updateNumericInput(session,inputId = "Nbblocks",
                       label = "Select the number of blocks:",
                       value = value, min = min, max = max ,step=1)

    updateRadioButtons(session,"whichRawSbmMatrix", "Select Ploted Matrix",
                       choices = list("Raw Matrix" = "raw",
                                      "Reordered Matrix" = "ordered",
                                      "Simplified Matrix" = "simple"),
                       selected = 'ordered')

    updateRadioButtons(session,"whichRawSbmNetwork", "Select Ploted Network:",
                       choices = list("Raw network" = "raw",
                                      "Ordered Network" = "ordered"),
                                      selected = 'ordered', inline = T)
  })


  my_sbm <- eventReactive(input$Nbblocks,{
    data_sbm <- my_sbm_main()$clone()
    data_sbm$setModel(input$Nbblocks)
    data_sbm
  })

  observeEvent(input$Nbblocks,{
    data_sbm <- my_sbm()$clone()
    data_sbm_main <- my_sbm_main()$clone()

    microplot <- ggplot2::ggplot(data_sbm$storedModels) + ggplot2::aes(x = nbBlocks, y = ICL)  +
      ggplot2::geom_line() + ggplot2::geom_point(alpha = 0.5) +
      ggplot2::geom_point(ggplot2::aes(x = data_sbm$nbBlocks, y = data_sbm$ICL, colour = 'b', size = 2)) +
      ggplot2::geom_point(ggplot2::aes(x = data_sbm_main$nbBlocks, y = data_sbm_main$ICL, colour = 'r', size = 2), shape = 10) +
      ggplot2::theme(legend.position = "none")
    output$showILC1 <- renderPlot({microplot})
    output$showILC2 <- renderPlot({microplot})
    output$showILC3 <- renderPlot({microplot})
  })



  output$sbmSummary <- renderPrint({
    data_sbm <- my_sbm()$clone()
    print(data_sbm$storedModels)
    print(c(NbBlockSelected = data_sbm$nbBlocks,
            min = min(data_sbm$storedModels$nbBlocks),
            max = max(data_sbm$storedModels$nbBlocks)))
  })

  PlotNet <- reactive({
    if(input$runSbm){
      data_sbm <- my_sbm()$clone()
      return(plot(data_sbm, type = "meso"))
    }else{
      return(NULL)
    }
  })

  output$networkPlot <- renderPlot({
    if(input$runSbm){
      data_sbm <- my_sbm()$clone()
      plot(data_sbm, type = "meso")
    }else{
      return(NULL)
    }
  })









  # shut down the app when it's closes on the browser
  session$onSessionEnded(function() {
    stopApp()
  })
}
