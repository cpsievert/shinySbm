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
  tagList(
    checkboxInput(ns("plotSbm_code"),label = strong("Show plorSbm Code"),value = TRUE),
    conditionalPanel("input.plotSbm_code % 2  == 0", ns = ns,
                     verbatimTextOutput(ns('plotSbm')))
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
}

## To be copied in the UI
# mod_show_code_ui("show_code_1")

## To be copied in the server
# mod_show_code_server("show_code_1")
