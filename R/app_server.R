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
    validate(
      need(datasetSelected(), "Please select a data set")
    )
    if(input$whichData == 'importData'){
      if(input$headerrow){
        x <- read.table(file = datasetSelected(), sep = sep(), row.names = 1, header = input$headercol)
      }else{
        x <- read.table(file = datasetSelected(), sep = sep(), header = input$headercol)
      }
      dataset <- buildSbmMatrix(x)
    }else{
      dataset <- switch(datasetSelected(),
                        "fungus_tree" = buildSbmMatrix(sbm::fungusTreeNetwork$fungus_tree, col_names = sbm::fungusTreeNetwork$tree_names,
                                                       row_names = sbm::fungusTreeNetwork$fungus_names),
                        "tree_tree" = buildSbmMatrix(sbm::fungusTreeNetwork$tree_tree, col_names = sbm::fungusTreeNetwork$tree_names,
                                                     row_names = sbm::fungusTreeNetwork$tree_names))
    }
    dataset
  })

  observeEvent(datasetUploaded(),{
    updateRadioButtons(session,"networkType",
                      "What kind of network it is ?",
                      choices = list("Bipartite" = "bipartite","Unipartite" = "unipartite"),
                      inline = T,
                      selected = datasetUploaded()$type)
    updateSelectInput(session,"whichLaw",
                      label = "What is the density expected upon dataset ?",
                      choices = list("Bernoulli" = "bernoulli",
                                     "Poisson" = "poisson",
                                     "Gaussian" = "gaussian"),
                      selected = datasetUploaded()$law)
  })

  workingDataset <- eventReactive(c(datasetUploaded(),input$networkType,input$whichLaw),{
      data <- datasetUploaded()
      data$type <- input$networkType
      data$law <- input$whichLaw
      data
  })

  output$warningDataImport1 <- renderPrint({
    warns <- list()
    withCallingHandlers(is.sbmMatrix(workingDataset(),warnings = T),
                        warning = function(w){warns <<- c(warns,list(w))})
    warning_messages <- sapply(warns,function(warn)warn$message)
    print_messages(warnings = warning_messages)
  })
  output$warningDataImport2 <- renderPrint({
    warns <- list()
    withCallingHandlers(is.sbmMatrix(workingDataset(),warnings = T),
                        warning = function(w){warns <<- c(warns,list(w))})
    warning_messages <- sapply(warns,function(warn)warn$message)
    print_messages(warnings = warning_messages)
  })
  output$summaryDataImport <- renderPrint({
    print(workingDataset())
  })





  output$matrixPrint <- DT::renderDataTable({
    # probleme : taille et position, wrapping des titres, fixer la colonnne de rownames
    req(input$whichShow)
    if(input$whichShow != 'print'){return(NULL)}
    DT::datatable(
      as.data.frame(workingDataset()),
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
      x <- as.matrix(workingDataset())
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
    datasetup <- workingDataset()
    data_res <- withProgress(message = "SBM is Running", {
      switch (input$networkType,
              "unipartite" = sbm::estimateSimpleSBM(netMat = as.matrix(datasetup), model = datasetup$law, estimOptions = list(verbosity = 0, plot = F)),
              "bipartite" = sbm::estimateBipartiteSBM(netMat = as.matrix(datasetup), model = datasetup$law, estimOptions = list(verbosity = 0, plot = F)))
    })
    return(data_res)
    })

  observeEvent(input$runSbm,{
    data_sbm <- my_sbm_main()$clone()
    value <- sum(data_sbm$nbBlocks)
    min <- min(data_sbm$storedModels$nbBlocks)
    max <- max(data_sbm$storedModels$nbBlocks)
    output$sbmCode <- renderText({
      switch (input$networkType,
              "unipartite" = paste0("mySbmModel <- sbm::estimateSimpleSBM(netMat = myNetworkMatrix, model = ",
                                    workingDataset()$law,", estimOptions = list(verbosity = 1))"),
              "bipartite" = paste0("mySbmModel <- sbm::estimateBipartiteSBM(netMat = myNetworkMatrix, model = '",
                                   workingDataset()$law,"', estimOptions = list(verbosity = 1))"))

    })
    updateNumericInput(session,inputId = "Nbblocks",
                       label = "Select the total number of blocks:",
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
    data_sbm$setModel(which(data_sbm$storedModels$nbBlocks == input$Nbblocks))
    data_sbm
  })

  observeEvent(c(input$Nbblocks,input$runSbm),{
    data_sbm <- my_sbm()$clone()
    data_sbm_main <- my_sbm_main()$clone()

    microplot <- ggplot2::ggplot(data_sbm$storedModels) + ggplot2::aes(x = nbBlocks, y = ICL,linetype = "ICL") +
      ggplot2::geom_line() + ggplot2::geom_point(alpha = 0.5) +
      ggplot2::geom_line(ggplot2::aes(x = nbBlocks, y = loglik,linetype = "Log Likelihood")) +
      ggplot2::geom_point(ggplot2::aes(x = sum(data_sbm$nbBlocks), y = data_sbm$ICL, colour = 'Selected Block Nb'),  size = 4) +
      ggplot2::geom_point(ggplot2::aes(x = sum(data_sbm_main$nbBlocks), y = data_sbm_main$ICL, colour = 'Best Block Nb'), size = 4, shape = 10) +
      ggplot2::labs(linetype = "Curves", colour = "Number of Blocks") +
      ggplot2::theme(
        legend.position = c(.40, .05),
        legend.justification = c("left", "bottom"),
        legend.box.just = "left",
        legend.margin = ggplot2::margin(6, 6, 6, 6)
      )
    output$showILC1 <- renderPlot({microplot})
    output$showILC2 <- renderPlot({microplot})
    output$showILC3 <- renderPlot({microplot})

    output$sbmSummarySelect <- renderPrint({
      data_sbm <- my_sbm()$clone()
      print(c(NbBlockSelected = data_sbm$nbBlocks))
    })

    output$sbmSummary <- renderPrint({
      data_sbm <- my_sbm()$clone()
      cat('Connectivity:\n')
      print(data_sbm$connectParam$mean)
      cat('\nBlock Proportions:\n')
      print(data_sbm$blockProp)
      cat('\nStored Model:\n')
      print(data_sbm$storedModels)
    })

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
