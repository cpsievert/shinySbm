#' tab_sbm UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_sbm_ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      width = 3,
      shinydashboard::box(
        title = "SBM settings", solidHeader = T,
        status = "info", width = 12,
        selectInput(ns("whichLaw"),
          label = "What is the density expected upon dataset ?",
          choices = list(
            "Bernoulli" = "bernoulli",
            "Poisson" = "poisson",
            "Gaussian" = "gaussian"
          ),
          selected = NULL
        ),
        uiOutput(ns("sbmButton"))
      ),
      mod_select_nb_groups_ui(ns("select_nb_groups_2"),12)
    ),
    column(
      width = 9,
      shinydashboard::box(
        title = "SBM outputs", solidHeader = T,
        status = "info", width = 12,
        strong("SBM code:"),
        verbatimTextOutput(ns("sbmCode")),
        mod_importation_error_ui(ns("error_2")),
        hr(),
        strong("SBM summary:"),
        verbatimTextOutput(ns("sbmSummarySelect")),
        tags$head(tags$style(paste0(
          "#", ns("sbmSummarySelect"),
          "{font-weight: bold}"
        ))),
        verbatimTextOutput(ns("sbmSummary"))
      )
    )
  )
}

#' tab_sbm Server Functions
#'
#' @noRd
mod_tab_sbm_server <- function(id, r, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$sbmButton <- renderUI({
      if (!is.null(r$upload$Dataset())) {
        tagList(
          hr(),
          div(
            style = "display:inline-block; float:right",
            actionButton(ns("runSbm"), strong("Run SBM"))
          )
        )
      }
    })


    Dataset <- eventReactive(c(r$upload$Dataset(), input$whichLaw), {
      data <- r$upload$Dataset()
      data$law <- input$whichLaw
      data
    })

    output$sbmCode <- renderText({
      switch(r$upload$networkType(),
        "unipartite" = paste0(
          "mySbmModel <- sbm::estimateSimpleSBM(netMat = myNetworkMatrix, model = ",
          Dataset()$law, ", estimOptions = list(verbosity = 1))"
        ),
        "bipartite" = paste0(
          "mySbmModel <- sbm::estimateBipartiteSBM(netMat = myNetworkMatrix, model = '",
          Dataset()$law, "', estimOptions = list(verbosity = 1))"
        )
      )
    })

    mod_importation_error_server("error_2", Dataset)

    my_sbm_main <- eventReactive(input$runSbm, {
      data_res <- withProgress(message = "SBM is Running", {
        switch(r$upload$networkType(),
          "unipartite" = sbm::estimateSimpleSBM(
            netMat = as.matrix(Dataset()),
            model = Dataset()$law, estimOptions = list(verbosity = 3, plot = F)
          ),
          "bipartite" = sbm::estimateBipartiteSBM(
            netMat = as.matrix(Dataset()),
            model = Dataset()$law, estimOptions = list(verbosity = 3, plot = F)
          )
        )
      })
      shinyalert::shinyalert(
        title = "SBM",
        text = "Calculation Done !",
        size = "xs",
        closeOnClickOutside = TRUE,
        type = "success",
        confirmButtonText = "OK"
      )
      return(data_res)
    })



    observeEvent(my_sbm_main(), {
      updateRadioButtons(parent_session, "tab_show_1-whichMatrix",
        "Select Ploted Matrix",
        choices = list(
          "Raw Matrix" = "raw",
          "Reordered Matrix" = "ordered",
          "Expected Matrix" = "expected"
        ),
        selected = "ordered"
      )
      updateRadioButtons(parent_session, "tab_network_1-whichNetwork",
        "Select Ploted Network",
        choices = list(
          "Raw Network" = "raw",
          "Grouped Network" = "grouped"
        ),
        selected = "grouped"
      )
    })


    mod_select_nb_groups_res <- mod_select_nb_groups_server(
      "select_nb_groups_2",
      my_sbm_main
    )

    my_sbm <- mod_select_nb_groups_res$my_sbm

    observeEvent(my_sbm(), {
      data_sbm <- my_sbm()$clone()
      output$sbmSummary <- renderPrint({
        options(digits = 2)
        cat("\nBlock Proportions:\n")
        print(data_sbm$blockProp)
        cat("Connectivity Betweens Blocks:\n")
        print(data_sbm$connectParam$mean)
        cat("\nModel Entropy (clustering quality):\n")
        print(data_sbm$entropy)
        cat("\nStored Models:\n")
        print(data_sbm$storedModels)
      })
    })

    return(list(
      Dataset = Dataset,
      NbBlocks = mod_select_nb_groups_res$Nbblocks,
      main_sbm = my_sbm_main,
      runSbm = reactive({
        input$runSbm
      })
    ))
  })
}



## To be copied in the UI
# mod_tab_sbm_ui("tab_sbm_1")

## To be copied in the server
# mod_tab_sbm_server("tab_sbm_1")
