#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  r <- reactiveValues(
    upload = reactiveValues(),
    show = reactiveValues(),
    sbm = reactiveValues(),
    network = reactiveValues(),
    extraction = reactiveValues()
  )

  ## Importing the data set
  r$upload <- mod_tab_upload_server("tab_upload_1", r,
    parent_session = session
  )

  ## Visualisation part
  r$show <- mod_tab_show_server("tab_show_1", r)

  ## SBM part
  r$sbm <- mod_tab_sbm_server("tab_sbm_1", r,
                              parent_session = session
                              )



  # observeEvent(input$runSbm, {
  #   data_sbm <- my_sbm_main()$clone()
  #   value <- sum(data_sbm$nbBlocks)
  #   min <- min(data_sbm$storedModels$nbBlocks)
  #   max <- max(data_sbm$storedModels$nbBlocks)


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
