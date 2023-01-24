#' importation_error UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_importation_error_ui <- function(id){
  ns <- NS(id)
  tagList(
    verbatimTextOutput(ns("warningDataImport")),
    tags$head(tags$style("#warningDataImport{color: red}"))
  )
}

#' importation_error Server Functions
#'
#' @noRd
mod_importation_error_server <- function(id,dataset){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$warningDataImport <- renderPrint({
      warns <- list()
      withCallingHandlers(is.sbmMatrix(dataset, warnings = T),
                          warning = function(w) {
                            warns <<- c(warns, list(w))
                          }
      )
      warning_messages <- sapply(warns, function(warn) warn$message)
      print_messages(warnings = warning_messages)
    })
  })
}

## To be copied in the UI
# mod_importation_error_ui("importation_error_1")

## To be copied in the server
# mod_importation_error_server("importation_error_1")
