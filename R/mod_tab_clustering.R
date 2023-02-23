#' tab_clustering UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_clustering_ui <- function(id) {
  ns <- NS(id)
  tagList(
    mod_select_nb_groups_ui(ns("select_nb_groups_4")),
    uiOutput(ns("namesGroups"))
  )
}

#' tab_clustering Server Functions
#'
#' @noRd
mod_tab_clustering_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    ns_tab_sbm <- function(id) {
      paste0("tab_sbm_1-", id)
    }


    my_sbm <- mod_select_nb_groups_server(
      "select_nb_groups_4",
      r$sbm$main_sbm
    )

    group_of_name <- reactive({
      getGroupNames(my_sbm(), r$sbm$Dataset())
    })

    output$trythis <- renderPrint({
      print(group_of_name()$liste)
    })

    # print(parent_session$input$`tab_sbm_1-runSbm`)

    # observe({
    #   mod_show_group_names_server("show_group_names",group_of_name()$listed_groups,1)
    #   })

    output$namesGroups <- renderUI({
      tagList(
        conditionalPanel(
          condition = "input.runSbm", ns = ns_tab_sbm,
          shinydashboard::box(
            title = "Groups", solidHeader = T,
            status = "info", collapsible = T, width = 9,
            verbatimTextOutput(ns("trythis"))
          )
        )
      )
    })
  })
}

## To be copied in the UI
# mod_tab_clustering_ui("tab_clustering_1")

## To be copied in the server
# mod_tab_clustering_server("tab_clustering_1")
