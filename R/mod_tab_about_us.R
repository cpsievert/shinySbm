#' tab_about_us UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_about_us_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$iframe(src = './myMarkdown.html', # put myMarkdown.html to /www
                width = '100%', height = '800px',
                frameborder = 0, scrolling = 'auto'
    )
  )
}

#' tab_about_us Server Functions
#'
#' @noRd
mod_tab_about_us_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_tab_about_us_ui("tab_about_us_1")

## To be copied in the server
# mod_tab_about_us_server("tab_about_us_1")
