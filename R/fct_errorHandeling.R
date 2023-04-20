
#' check_data_inputs
#'
#' @description Check the raw data.frame according to inputs
#'
#' @param dta=NULL,inputs=NULL
#' `dta` data.frame
#' `inputs` list of inputs from upload table :
#' - `input$whichData`
#' - `input$dataType`
#' - `input$headerrow`
#' - `input$headercol`
#' - `input$orientation`
#' - `input$networkType`
#'
#' @return Based on those inputs and the data.frame, it give proper messages
#' and warnings with indication to get a better results
#'
#' @noRd
check_data_inputs <- function(dta = NULL, inputs = NULL){
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






#' print_messages
#'
#' @description print stored messages, warnings and errors
#'
#' @param messages,warnings,errors
#' `messages` stored in a list
#' `warnings` stored in a list
#' `errors` stored in a list
#'
#' @return beautifull cat
#'
#' @noRd
print_messages <- function(messages = list(), warnings = list(), errors = list()) {
  if (!identical(messages, list())) {
    cat("Messages :\n")
    for (i in 1:length(messages)) {
      cat("[", i, "] ", messages[[i]], "\n", sep = "")
    }
  }
  if (!identical(warnings, list())) {
    cat("Warnings :\n")
    for (i in 1:length(warnings)) {
      cat("[", i, "] ", warnings[[i]], "\n", sep = "")
    }
  }
  if (!identical(errors, list())) {
    cat("Errors :\n")
    for (i in 1:length(errors)) {
      cat("[", i, "] ", errors[[i]], "\n", sep = "")
    }
  }
}
