#' tab_show UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tab_show_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' tab_show Server Functions
#'
#' @noRd 
mod_tab_show_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_tab_show_ui("tab_show_1")
    
## To be copied in the server
# mod_tab_show_server("tab_show_1")
