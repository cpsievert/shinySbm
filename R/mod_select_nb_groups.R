#' select_nb_groups UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_select_nb_groups_ui <- function(id){
  ns <- NS(id)
  tagList(
    numericInput(ns("Nbblocks"),
                 label = "Select the total number of blocks:",
                 value = 4, min = 1, max = 6, step = 1
    ),
    plotOutput(ns("showILC"))
  )
}

#' select_nb_groups Server Functions
#'
#' @noRd
mod_select_nb_groups_server <- function(id, my_sbm_main, runSbm){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      data_sbm <- my_sbm_main()$clone()
      value <- sum(data_sbm$nbBlocks)
      min <- min(data_sbm$storedModels$nbBlocks)
      max <- max(data_sbm$storedModels$nbBlocks)
      updateNumericInput(session,
                         inputId = "Nbblocks",
                         label = "Select the total number of blocks:",
                         value = value, min = min, max = max, step = 1)
    })

    my_sbm <- eventReactive(input$Nbblocks, {
      data_sbm <- my_sbm_main()$clone()
      min <- min(data_sbm$storedModels$nbBlocks)
      max <- max(data_sbm$storedModels$nbBlocks)

      if (input$Nbblocks %in% min:max) {
        data_sbm$setModel(which(data_sbm$storedModels$nbBlocks == input$Nbblocks))
      }
      data_sbm
    })

    observeEvent(c(input$Nbblocks, runSbm), {
        data_sbm <- my_sbm()$clone()
        data_sbm_main <- my_sbm_main()$clone()
        output$showILC <- renderPlot({
          ILC_plot(data_sbm,data_sbm_main)
        })
    })
    return(list(Nbblocks = reactive({input$Nbblocks}),
                my_sbm = my_sbm))
  })
}

## To be copied in the UI
# mod_select_nb_groups_ui("select_nb_groups_1")

## To be copied in the server
# mod_select_nb_groups_server("select_nb_groups_1")
