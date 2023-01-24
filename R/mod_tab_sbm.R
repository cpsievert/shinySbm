#' tab_sbm UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tab_sbm_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' tab_sbm Server Functions
#'
#' @noRd 
mod_tab_sbm_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_tab_sbm_ui("tab_sbm_1")
    
## To be copied in the server
# mod_tab_sbm_server("tab_sbm_1")
