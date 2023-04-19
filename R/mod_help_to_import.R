#' help_to_import UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_help_to_import_ui <- function(id){
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
mod_help_to_import_server <- function(id, rawData = NULL, input_upload = NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_help_to_import_ui("help_to_import_1")

## To be copied in the server
# mod_help_to_import_server("help_to_import_1")
