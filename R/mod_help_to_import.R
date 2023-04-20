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
mod_help_to_import_server <- function(id, rawData, input_upload){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    my_check <- function(dta = NULL, inputs = NULL){
      ## Check for all cases
      if(dim(dta)[2]<=1){
        warning("Low number of columns : Try to change separator")
      }else{
        if(inputs$headerrow & any(duplicated(dta[[1]])) & is.character(dta[[1]])){
          message("Repeated values in the 1st column : cannot be set as row names")
          }
        ## Check in case of a adjacency matrix
        if(inputs$dataType == 'matrix'){
          if(any(sapply(dta,is.character))){
            if(!inputs$headercol | !inputs$headerrow){
              warning("Some characters in matrix : Try with 1st column and/or row as names")
            }else{
              warning("Some characters in matrix : Check if your data is correctly encoded")
            }
          }
        }else{ ## Check in case of a list or node pairs
          if(dim(dta)[2]>3){
            warning("Data set is wider than 2 or 3 columns : Are you sure it's a list of node pairs and not a matrix ?")
          }else{
            if(dim(dta)[2] == 3 & !is.numeric(dta[[3]])){
              if(!inputs$headercol){
                warning("3rd column of a node pairs list can only be numeric : Try with 1st column as names")
              }else{
                warning("3rd column of a node pairs list can only be numeric : Check if your data is correctly encoded")
              }

            }
          }
        }
      }
    }




    warn_list <- reactiveValues(messages = list(),
                                warnings = list())

    observe({
      if(input_upload$whichData == 'importData'){
        warns <- list()
        mess <- list()
        withCallingHandlers(my_check(dta = rawData(), inputs = input_upload),
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
