#' select_nb_groups UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_select_nb_groups_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' select_nb_groups Server Functions
#'
#' @noRd 
mod_select_nb_groups_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_select_nb_groups_ui("select_nb_groups_1")
    
## To be copied in the server
# mod_select_nb_groups_server("select_nb_groups_1")
