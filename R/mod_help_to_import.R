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
    uiOutput(ns("help")),
    verbatimTextOutput(ns("messageDataImport")),
    verbatimTextOutput(ns("warningDataImport")),
    tags$head(tags$style(paste0("#", ns("warningDataImport"), "{color: red}")))
  )
}

#' help_to_import Server Functions
#'
#' @noRd
mod_help_to_import_server <- function(id, rawData = NULL, sbmData = NULL, input_upload = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    warn_list <- reactiveValues(
      messages = list(),
      warnings = list()
    )

    # Check if changes have been loaded
    unloaded_changes <- reactiveValues(v = F)
    observe({
      input_upload$matrixBuilder
      unloaded_changes$v <- F
    })
    observe({
      input_upload$whichData
      input_upload$dataBase
      input_upload$dataType
      input_upload$headercol
      input_upload$headerrow
      input_upload$orientation
      input_upload$networkType
      if(identical(warn_list$warnings,list())){
        if(!is.null(rawData) && "data.frame" %in% class(rawData())){
          unloaded_changes$v <- T
        }
      }else{
        unloaded_changes$v <- F
      }
    })


    observe({
      if(!is.null(rawData) & !is.null(input_upload) && input_upload$whichData == "importData"){
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


    observe({
      if(!is.null(input_upload) & !is.null(sbmData)){
        warns <- list()
        mess <- list()
        withCallingHandlers(check_sbm_inputs(sbmData = sbmData(), inputs = input_upload),
                            warning = function(w) {
                              warns <<- c(warns, list(w))
                            },
                            message = function(m) {
                              mess <<- c(mess, list(m))
                            }
        )
        warn_list$messages <- sapply(mess, function(mess) mess$message)
        warn_list$warnings <- sapply(warns, function(warn) warn$message)
      }else if(!is.null(sbmData)){
        warns <- list()
        mess <- list()
        withCallingHandlers(is.sbmMatrix(sbmData(), warnings = T),
                            warning = function(w) {
                              warns <<- c(warns, list(w))
                            },
                            message = function(m) {
                              mess <<- c(mess, list(m))
                            }
        )
        warn_list$messages <- sapply(mess, function(mess) mess$message)
        warn_list$warnings <- sapply(warns, function(warn) warn$message)
      }else{
        warn_list$messages <- list()
        warn_list$warnings <- list()
      }
    })

    observe({
      if(!identical(warn_list$messages,list()) | !identical(warn_list$warnings,list())){
        output$help <- renderUI({strong('Help:')})
      }else{
        output$help <- renderUI({})
      }
    })

    output$messageDataImport <- renderPrint({
      if(unloaded_changes$v){
        cat("For Importation press : Matrix Builder\n")
      }
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
