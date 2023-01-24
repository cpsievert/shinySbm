#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  ## Importing the data set
  tab_upload_res <- mod_tab_upload_server("tab_upload_1")
  workingDataset <- tab_upload_res$workingDataset
  labels <- tab_upload_res$labels

  ## Visualisation part
  mod_tab_show_server("tab_show_1", workingDataset, labels)

  my_sbm_main <- eventReactive(input$runSbm, {
    datasetup <- workingDataset()
    data_res <- withProgress(message = "SBM is Running", {
      switch(input$networkType,
        "unipartite" = sbm::estimateSimpleSBM(
          netMat = as.matrix(datasetup),
          model = datasetup$law, estimOptions = list(verbosity = 3, plot = T)
        ),
        "bipartite" = sbm::estimateBipartiteSBM(
          netMat = as.matrix(datasetup),
          model = datasetup$law, estimOptions = list(verbosity = 3, plot = T)
        )
      )
    })
    return(data_res)
  })

  observeEvent(input$runSbm, {
    data_sbm <- my_sbm_main()$clone()
    value <- sum(data_sbm$nbBlocks)
    min <- min(data_sbm$storedModels$nbBlocks)
    max <- max(data_sbm$storedModels$nbBlocks)
    output$sbmCode <- renderText({
      switch(input$networkType,
        "unipartite" = paste0(
          "mySbmModel <- sbm::estimateSimpleSBM(netMat = myNetworkMatrix, model = ",
          workingDataset()$law, ", estimOptions = list(verbosity = 1))"
        ),
        "bipartite" = paste0(
          "mySbmModel <- sbm::estimateBipartiteSBM(netMat = myNetworkMatrix, model = '",
          workingDataset()$law, "', estimOptions = list(verbosity = 1))"
        )
      )
    })
    updateNumericInput(session,
      inputId = "Nbblocks",
      label = "Select the total number of blocks:",
      value = value, min = min, max = max, step = 1
    )

    # updateRadioButtons(session, "whichRawSbmMatrix", "Select Ploted Matrix",
    #   choices = list(
    #     "Raw Matrix" = "raw",
    #     "Reordered Matrix" = "ordered",
    #     "Simplified Matrix" = "simple"
    #   ),
    #   selected = "ordered"
    # )
    #
    # updateRadioButtons(session, "whichRawSbmNetwork", "Select Ploted Network:",
    #   choices = list(
    #     "Raw network" = "raw",
    #     "Ordered Network" = "ordered"
    #   ),
    #   selected = "ordered", inline = T
    # )
  })

  my_sbm <- eventReactive(input$Nbblocks, {
    data_sbm <- my_sbm_main()$clone()
    min <- min(data_sbm$storedModels$nbBlocks)
    max <- max(data_sbm$storedModels$nbBlocks)
    if (input$Nbblocks %in% min:max) {
      data_sbm$setModel(which(data_sbm$storedModels$nbBlocks == input$Nbblocks))
    }
    data_sbm
  })

  observeEvent(c(input$Nbblocks, input$runSbm), {
    data_sbm <- my_sbm()$clone()
    data_sbm_main <- my_sbm_main()$clone()

    microplot <- ggplot2::ggplot(data_sbm$storedModels) +
      ggplot2::aes(x = nbBlocks, y = ICL, linetype = "ICL") +
      ggplot2::geom_line() +
      ggplot2::geom_point(alpha = 0.5) +
      ggplot2::geom_line(ggplot2::aes(x = nbBlocks, y = loglik, linetype = "Log Likelihood")) +
      ggplot2::geom_point(ggplot2::aes(x = sum(data_sbm$nbBlocks), y = data_sbm$ICL, colour = "Selected Block Nb"), size = 4) +
      ggplot2::geom_point(ggplot2::aes(x = sum(data_sbm_main$nbBlocks), y = data_sbm_main$ICL, colour = "Best Block Nb"), size = 4, shape = 10) +
      ggplot2::labs(linetype = "Curves", colour = "Number of Blocks") +
      ggplot2::theme(
        legend.position = c(.40, .05),
        legend.justification = c("left", "bottom"),
        legend.box.just = "left",
        legend.margin = ggplot2::margin(6, 6, 6, 6)
      )
    output$showILC1 <- renderPlot({
      microplot
    })
    output$showILC2 <- renderPlot({
      microplot
    })
    output$showILC3 <- renderPlot({
      microplot
    })

    output$sbmSummarySelect <- renderPrint({
      data_sbm <- my_sbm()$clone()
      print(c(NbBlockSelected = data_sbm$nbBlocks))
    })

    output$sbmSummary <- renderPrint({
      data_sbm <- my_sbm()$clone()
      cat("Connectivity:\n")
      print(data_sbm$connectParam$mean)
      cat("\nBlock Proportions:\n")
      print(data_sbm$blockProp)
      cat("\nStored Model:\n")
      print(data_sbm$storedModels)
    })
  })

  PlotNet <- reactive({
    if (input$runSbm) {
      data_sbm <- my_sbm()$clone()
      return(plot(data_sbm, type = "meso"))
    } else {
      return(NULL)
    }
  })

  output$networkPlot <- renderPlot({
    if (input$runSbm) {
      data_sbm <- my_sbm()$clone()
      plot(data_sbm, type = "meso")
    } else {
      return(NULL)
    }
  })

  # shut down the app when it's closes on the browser
  session$onSessionEnded(function() {
    stopApp()
  })
}
