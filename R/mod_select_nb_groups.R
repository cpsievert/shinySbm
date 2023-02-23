#' select_nb_groups UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_select_nb_groups_ui <- function(id, wind_width = 3) {
  ns <- NS(id)
  ns_tab_sbm <- function(id) {
    paste0("tab_sbm_1-", id)
  }
  ns_tab_upload <- function(id) {
    paste0("tab_upload_1-", id)
  }
  tagList(
    conditionalPanel(
      condition = "output.sbmRan == 'YES'",
      ns = ns_tab_upload,
      shinydashboard::box(
        title = actionLink(
          inputId = ns("showGraph"),
          label = "Block settings",
          icon = icon("magnifying-glass-minus")
        ),
        solidHeader = T,
        status = "info", collapsible = F, width = wind_width,
        numericInput(ns("Nbblocks"),
          label = "Select the total number of blocks:",
          value = 4, min = 1, max = 6, step = 1
        ),
        conditionalPanel(
          condition = "input.showGraph % 2 == 0", ns = ns,
          plotOutput(ns("showILC"))
        )
      )
    )
  )
}

#' select_nb_groups Server Functions
#'
#' @noRd
mod_select_nb_groups_server <- function(id, my_sbm_main) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    observeEvent(session$userData$vars$tab(),{
      if(session$userData$vars$sbm$NbBlocks != input$Nbblocks){
        updateNumericInput(session,
                           inputId = "Nbblocks",
                           label = "Select the total number of blocks:",
                           value = session$userData$vars$sbm$NbBlocks
        )
      }
    })

    observeEvent(input$showGraph, {
      if (input$showGraph %% 2 == 0) {
        updateActionLink(session,
          inputId = "showGraph",
          icon = icon("magnifying-glass-minus")
        )
      } else {
        updateActionLink(session,
          inputId = "showGraph",
          icon = icon("magnifying-glass-plus")
        )
      }
    })

    observeEvent(my_sbm_main(), {
      data_sbm <- my_sbm_main()$clone()
      value <- sum(data_sbm$nbBlocks)
      min <- min(data_sbm$storedModels$nbBlocks)
      max <- max(data_sbm$storedModels$nbBlocks)
      updateNumericInput(session,
                         inputId = "Nbblocks",
                         label = "Select the total number of blocks:",
                         value = value, min = min, max = max, step = 1
      )
      session$userData$vars$sbm$NbBlocks <- value
    })

    my_sbm <- eventReactive(session$userData$vars$sbm$NbBlocks, {
      n <- session$userData$vars$sbm$NbBlocks
      data_sbm <- my_sbm_main()$clone()
      min <- min(data_sbm$storedModels$nbBlocks)
      max <- max(data_sbm$storedModels$nbBlocks)

      if (n %in% min:max) {
        data_sbm$setModel(which(data_sbm$storedModels$nbBlocks == n))
      }
      data_sbm
    })

    observeEvent(input$Nbblocks,{
      session$userData$vars$sbm$NbBlocks <- input$Nbblocks
    })


    observeEvent(c(input$Nbblocks, my_sbm_main()), {
      data_sbm <- my_sbm()$clone()
      data_sbm_main <- my_sbm_main()$clone()
      output$showILC <- renderPlot({
        ILC_plot(data_sbm, data_sbm_main)
      })
    })

    return(my_sbm)
  })
}

## To be copied in the UI
# mod_select_nb_groups_ui("select_nb_groups_1")

## To be copied in the server
# mod_select_nb_groups_server("select_nb_groups_1")
