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
    checkboxInput(ns("visSbm_code"),label = strong("Show visSbm Code"),value = TRUE),
    conditionalPanel("input.visSbm_code % 2  == 0", ns = ns,
                     verbatimTextOutput(ns('visCode')))
  )
}

#' network_code Server Functions
#'
#' @noRd
mod_network_code_server <- function(id,settings,upload,node_colors,node_shapes,thresh_default){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    netPlot_code <- reactiveValues(
      printNet = character()
    )

    netPlot_text <- reactive({
      paste(
        c(
          netPlot_code$printNet
        ),
        collapse = "\n"
      )
    })
    session$userData$netPlot_code <- netPlot_text


    observe({
      if((!is.null(settings$arrows) && settings$arrows == 'FALSE') & (is.null(settings$arrow_start) || settings$arrow_start == "")){
        arrow_start <- ''
      }else{
        arrow_start <- paste0("    arrow_start = '",settings$arrow_start,"',\n")
      }
      if(upload$networkType() == "bipartite"){
        labels <- paste0("c(row = '",upload$labels()$row,
                         "', col = '",upload$labels()$col,"')")
        node_color <- paste0("c(row = '",node_colors()$row,
                             "', col = '",node_colors()$col,"')")
        node_shape <- paste0("c(row = '",node_shapes()$row,
                             "', col = '",node_shapes()$col,"')")
      }else{
        labels <- paste0("'",upload$labels()$row,"'")
        node_color <- paste0("'",node_colors()[[1]],"'")
        node_shape <- paste0("'",node_shapes()[[1]],"'")
      }
      if(thresh_default()){
        edge_thresh <- "    edge_threshold = 'default',\n"
      }else{
        edge_thresh <-paste0("    edge_threshold = ",settings$edge_threshold,",\n")
      }
      netPlot_code$printNet <- paste0(
        "visSbm(","\n",
        "  x = mySbmModel",",\n",
        "  labels = ",labels,",\n",
        "  directed = ",upload$directed(),",\n",
        "  settings = list(","\n",
        edge_thresh,
        "    edge_color = '",settings$edge_color,"',\n",
        "    arrows = ",settings$arrows,",\n",
        arrow_start,
        "    node_color = ",node_color,",\n",
        "    node_shape = ",node_shape,"\n",
        "  ))",
        sep = '\n'
      )
    })










    output$visCode <- renderPrint({
      cat(netPlot_text())
    })
  })
}

## To be copied in the UI
# mod_network_code_ui("network_code_1")

## To be copied in the server
# mod_network_code_server("network_code_1")
