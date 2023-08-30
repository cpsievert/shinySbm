#' show_code UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_show_code_ui <- function(id){
  ns <- NS(id)
  ns_tab_show <- function(id) {
    paste0("tab_show_1-", id)
  }
  tagList(
    conditionalPanel(condition = "input.whichShow == 'plot'",
                     ns = ns_tab_show,
                     checkboxInput(ns("plotSbm_code"),label = strong("Show plorSbm Code"),value = TRUE),
                     conditionalPanel("input.plotSbm_code % 2  == 0", ns = ns,
                                      verbatimTextOutput(ns('plotSbm')))
                     )

  )
}

#' show_code Server Functions
#'
#' @noRd
mod_show_code_server <- function(id,settings,upload){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    matPlot_code <- reactiveValues(
      matPlot = character()
    )

    matPlot_text <- reactive({
      paste(
        c(
          matPlot_code$matPlot
        ),
        collapse = "\n"
      )
    })
    session$userData$matPlot_code <- matPlot_text
  })


  # PlotMat <- reactive({
  #   req(input$whichShow)
  #   if (input$whichShow == "plot") {
  #     x <- as.matrix(r$upload$Dataset())
  #     labels_list <- r$upload$labels()
  #     my_Options <- list(
  #       title = if (input$setTitle == "") {
  #         NULL
  #       } else {
  #         input$setTitle
  #       },
  #       showLegend = input$showLegend,
  #       showPredictions = input$showPred,
  #       colPred = input$colorPred,
  #       colValue = input$colorValues,
  #       interactionName = input$interactionName
  #     )
  #
  #     if (session$userData$vars$sbm$runSbm != 0) {
  #       data_sbm <- my_sbm()$clone()
  #       switch(input$whichMatrix,
  #              "raw" = plotSbm(data_sbm,
  #                              ordered = FALSE, transpose = input$showTransposed,
  #                              labels = labels_list,
  #                              plotOptions = my_Options
  #              ),
  #              "ordered" = plotSbm(data_sbm,
  #                                  ordered = TRUE, transpose = input$showTransposed,
  #                                  labels = labels_list,
  #                                  plotOptions = my_Options
  #              ),
  #              "expected" = plotSbm(data_sbm,
  #                                   ordered = TRUE, transpose = input$showTransposed,
  #                                   labels = labels_list,
  #                                   plotOptions = c(my_Options, showValues = F)
  #              )
  #       )
  #     } else {
  #       plotSbm(x,
  #               transpose = input$showTransposed,
  #               labels = labels_list, plotOptions = my_Options
  #       )
  #     }
  #   } else {
  #     return(NULL)
  #   }
  # })
}

## To be copied in the UI
# mod_show_code_ui("show_code_1")

## To be copied in the server
# mod_show_code_server("show_code_1")
