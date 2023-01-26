#' tab_network UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_network_ui <- function(id) {
  ns <- NS(id)
  ns_tab_sbm <- function(id) {
    paste0("tab_sbm_1-", id)
  }
  tagList(
    shinydashboard::box(
      title = "Visual settings", solidHeader = T,
      status = "info", collapsible = T,
      radioButtons(ns("whichNetwork"), "Select Ploted Network",
        choices = list("Raw Network" = "raw")
      )
    ),
    conditionalPanel(
      condition = "input.runSbm", ns = ns_tab_sbm,
      shinydashboard::box(
        title = "Block settings", solidHeader = T,
        status = "info", collapsible = T, width = 3,
        mod_select_nb_groups_ui(ns("select_nb_groups_3"))
      )
    ),
    shinydashboard::box(
      title = "Network", solidHeader = T,
      status = "info", width = 12,
      visNetwork::visNetworkOutput(ns("networkPlot"))
    )
  )
}

#' tab_network Server Functions
#'
#' @noRd
mod_tab_network_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    mod_select_nb_groups_res <- mod_select_nb_groups_server(
      "select_nb_groups_3",
      r$sbm$main_sbm, r$sbm$NbBlocks
    )
    my_sbm <- mod_select_nb_groups_res$my_sbm

    node_edge <- reactive({
      build_node_edge(my_sbm())
    })

    output$networkPlot <- visNetwork::renderVisNetwork({
      if (input$whichNetwork != "raw") {
        local_value <- node_edge()
        netPlot(local_value$nodes, local_value$edges, local_value$type)
      } else {
        return(NULL)
      }
    })

    return(list(
      NbBlocks = mod_select_nb_groups_res$Nbblocks
    ))
  })
}

## To be copied in the UI
# mod_tab_network_ui("tab_network_1")

## To be copied in the server
# mod_tab_network_server("tab_network_1")
