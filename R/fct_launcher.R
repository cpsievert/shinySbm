#' SBMshiny
#'
#' @description A fonction launching the app using an arugment from your R session
#'
#' @param network Network data of different format available
#'
#' @return Nothing
#'
#' @export
SBMshiny <- function(network = NULL){
  .GlobalEnv$.intitial.network <- network
  on.exit(rm(.intitial.network, envir=.GlobalEnv))
  runApp()
}
