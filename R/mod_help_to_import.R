#' help_to_import UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_help_to_import_ui <- function(id) {
  ns <- NS(id)
  tagList(
    verbatimTextOutput(ns("messageDataImport")),
    verbatimTextOutput(ns("warningDataImport")),
    tags$head(tags$style(paste0("#", ns("warningDataImport"), "{color: red}")))
  )
}

#' help_to_import Server Functions
#'
#' @noRd
mod_help_to_import_server <- function(id, rawData, input_upload) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    warn_list <- reactiveValues(
      messages = list(),
      warnings = list()
    )

    observe({
      if (input_upload$whichData == "importData") {
        warns <- list()
        mess <- list()
        withCallingHandlers(check_data_inputs(dta = rawData(), inputs = input_upload),
          warning = function(w) {
            warns <<- c(warns, list(w))
          },
          message = function(m) {
            mess <<- c(mess, list(m))
          }
        )
        warn_list$messages <- sapply(mess, function(mess) mess$message)
        warn_list$warnings <- sapply(warns, function(warn) warn$message)
      } else {
        warn_list$messages <- list()
        warn_list$warnings <- list()
      }
    })




    output$messageDataImport <- renderPrint({
      print_messages(messages = warn_list$messages)
    })
    output$warningDataImport <- renderPrint({
      print_messages(warnings = warn_list$warnings)
    })
  })
}

## To be copied in the UI
# mod_help_to_import_ui("help_to_import_1")

## To be copied in the server
# mod_help_to_import_server("help_to_import_1")
