#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  ## Importing the data set
  tab_upload_res <- mod_tab_upload_server("tab_upload_1",
                                          parent_session = session)

  workingDataset <- tab_upload_res$workingDataset
  labels <- tab_upload_res$labels
  networkType <- tab_upload_res$networkType

  ## Visualisation part
  mod_tab_show_server("tab_show_1", workingDataset, labels)

  ## SBM part
  tab_sbm_res <- mod_tab_sbm_server("tab_sbm_1",workingDataset,networkType)

  my_sbm <- tab_sbm_res$sbm
  my_sbm_main <- tab_sbm_res$main_sbm

  # observeEvent(input$runSbm, {
  #   data_sbm <- my_sbm_main()$clone()
  #   value <- sum(data_sbm$nbBlocks)
  #   min <- min(data_sbm$storedModels$nbBlocks)
  #   max <- max(data_sbm$storedModels$nbBlocks)

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
  # })


  # observeEvent(c(input$Nbblocks, input$runSbm), {
  #   data_sbm <- my_sbm()$clone()
  #   data_sbm_main <- my_sbm_main()$clone()
  # ILC_plot(data_sbm,data_sbm_main)
  #   output$showILC1 <- renderPlot({
  #     microplot
  #   })
  #   output$showILC2 <- renderPlot({
  #     microplot
  #   })
  #   output$showILC3 <- renderPlot({
  #     microplot
  #   })
  #
  #   output$sbmSummarySelect <- renderPrint({
  #     data_sbm <- my_sbm()$clone()
  #     print(c(NbBlockSelected = data_sbm$nbBlocks))
  #   })
  #
  #   output$sbmSummary <- renderPrint({
  #     data_sbm <- my_sbm()$clone()
  #     cat("Connectivity:\n")
  #     print(data_sbm$connectParam$mean)
  #     cat("\nBlock Proportions:\n")
  #     print(data_sbm$blockProp)
  #     cat("\nStored Model:\n")
  #     print(data_sbm$storedModels)
  #   })
  # })
  #
  # PlotNet <- reactive({
  #   if (input$runSbm) {
  #     data_sbm <- my_sbm()$clone()
  #     return(plot(data_sbm, type = "meso"))
  #   } else {
  #     return(NULL)
  #   }
  # })
  #
  # output$networkPlot <- renderPlot({
  #   if (input$runSbm) {
  #     data_sbm <- my_sbm()$clone()
  #     plot(data_sbm, type = "meso")
  #   } else {
  #     return(NULL)
  #   }
  # })

  # shut down the app when it's closes on the browser
  session$onSessionEnded(function() {
    stopApp()
  })
}
