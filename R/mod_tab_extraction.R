#' tab_extraction UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_extraction_ui <- function(id) {
  ns <- NS(id)
  ns_tab_sbm <- function(id) {
    paste0("tab_sbm_1-", id)
  }
  tagList(
    conditionalPanel(
      condition = "input.runSbm", ns = ns_tab_sbm,
      shinydashboard::box(
        title = "Block settings", solidHeader = T,
        status = "info", collapsible = T, width = 3,
        mod_select_nb_groups_ui(ns("select_nb_groups_4"))
      )
    ),
    uiOutput(ns("namesGroups"))
  )
}

#' tab_extraction Server Functions
#'
#' @noRd
mod_tab_extraction_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    mod_select_nb_groups_res <- mod_select_nb_groups_server(
      "select_nb_groups_4",
      r$sbm$main_sbm, r$sbm$NbBlocks
    )
    my_sbm <- mod_select_nb_groups_res$my_sbm

    group_of_name <- eventReactive(my_sbm(),{
      getGroupNames(my_sbm(), r$sbm$Dataset())
    })

    mod_show_group_names_server("show_group_names",group_of_name(),1)


    output$namesGroups <- renderUI({
      tagList(
        mod_show_group_names_ui(ns("show_group_names"))
      )
    })
    return(list(NbBlocks = mod_select_nb_groups_res$Nbblocks))
  })
}

## To be copied in the UI
# mod_tab_extraction_ui("tab_extraction_1")

## To be copied in the server
# mod_tab_extraction_server("tab_extraction_1")
