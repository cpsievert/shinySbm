#' tab_extraction UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_extraction_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      title = "Parameters", solidHeader = T,
      status = "info", collapsible = T,
      textInput(ns("fileName"),
                label = "File Name",
                value = "Group_list"
      ),
      radioButtons(ns("fileType"), "Select file type:",
                   choices = list(
                     "csv" = ".csv",
                     "txt" = ".txt"
                   ),
                   selected = ".csv",
                   inline = T
      ),
      downloadButton(ns("downData"),label = 'Extract Data')
    ),
    mod_select_nb_groups_ui(ns("select_nb_groups_5"))
  )
}

#' tab_extraction Server Functions
#'
#' @noRd
mod_tab_extraction_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    my_sbm <- mod_select_nb_groups_server(
      "select_nb_groups_5",
      r$sbm$main_sbm,
      session
    )

  })
}

## To be copied in the UI
# mod_tab_extraction_ui("tab_extraction_1")

## To be copied in the server
# mod_tab_extraction_server("tab_extraction_1")
