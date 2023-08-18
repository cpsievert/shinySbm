#' network_code UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_network_code_ui <- function(id){
  ns <- NS(id)
  tagList(
    strong("visSbm Code:"),
    verbatimTextOutput(ns('visCode')),
    br()
  )
}

#' network_code Server Functions
#'
#' @noRd
mod_network_code_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # node_colors()
    # node_shapes()

    # inputs
    # input$edge_threshold see to put default value also
    # input$edge_color
    # input$arrows
    # input$arrow_start

    # r$upload$labels() do something before ?
    # r$upload$Dataset() how to do ?
    # r$upload$directed()

    # visSbm(
    #   x = mySbmModel,
    #   labels = r$upload$labels(),
    #   node_names = r$upload$Dataset(),
    #   directed = r$upload$directed(),
    #   settings = list(
    #     edge_threshold = input$edge_threshold,
    #     edge_color = input$edge_color,
    #     arrows = input$arrows,
    #     arrow_start = input$arrow_start,
    #     node_color = node_colors(),
    #     node_shape = node_shapes()
    #   )


    output$visCode <- renderPrint({
    })
  })
}

## To be copied in the UI
# mod_network_code_ui("network_code_1")

## To be copied in the server
# mod_network_code_server("network_code_1")
