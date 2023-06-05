#' tab_about_us UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_about_us_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      title = "Description",
      solidHeader = T, status = "info",
      collapsible = T, collapsed = T,
      tags$iframe(
        src = "www/README.html", # put myMarkdown.html to /www
        width = "100%", height = "800px",
        frameborder = 0, scrolling = "auto"
      )
    )
  )
}

#' tab_about_us Server Functions
#'
#' @noRd
mod_tab_about_us_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_tab_about_us_ui("tab_about_us_1")

## To be copied in the server
# mod_tab_about_us_server("tab_about_us_1")



check_package <- function(package,version){
  is_installed <- 'dplyr' %in% rownames(installed.packages())
  is_upper <- is_installed &&
    utils::compareVersion(installed.packages()['dplyr','Version'],version) >= 0
  if(!is_installed){
    stop("Please install ",package," package first")
  }
  if(!is_upper){
    stop(package,' version is lower than ',version,"\nThis version is mandatory, please install it")
  }
  return(T)
}

