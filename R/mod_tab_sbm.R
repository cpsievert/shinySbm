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
        status = "info",width = 12,
        selectInput(ns("whichLaw"),
          label = "What is the density expected upon dataset ?",
          choices = list(
            "Bernoulli" = "bernoulli",
            "Poisson" = "poisson",
            "Gaussian" = "gaussian"
          ),
          selected = NULL
        ),
        div(
          style = "display:inline-block; float:right",
        actionButton(ns("runSbm"), strong("Run SBM"))
        )
      ),
      conditionalPanel(
        condition = "input.runSbm", ns = ns,
        shinydashboard::box(
          title = "Block Selection", solidHeader = T,
          status = "info",width = 12,
          mod_select_nb_groups_ui(ns("select_nb_groups_2"))
        )
      )
    ),
    column(
      width = 9,
      shinydashboard::box(
        title = "SBM outputs", solidHeader = T,
        status = "info",width = 12,
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
mod_tab_sbm_server <- function(id, workingDataset, networkType) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    mod_importation_error_server("error_2", workingDataset)

    my_sbm_main <- eventReactive(input$runSbm, {
      datasetup <- workingDataset()
      data_res <- withProgress(message = "SBM is Running", {
        switch(networkType(),
               "unipartite" = sbm::estimateSimpleSBM(
                 netMat = as.matrix(datasetup),
                 model = datasetup$law, estimOptions = list(verbosity = 0, plot = F)
               ),
               "bipartite" = sbm::estimateBipartiteSBM(
                 netMat = as.matrix(datasetup),
                 model = datasetup$law, estimOptions = list(verbosity = 0, plot = F)
               )
        )
      })
      return(data_res)
    })

    my_sbm <- my_sbm_main

    output$sbmSummary <- renderPrint({
      data_sbm <- my_sbm()$clone()
      cat("Connectivity:\n")
      print(data_sbm$connectParam$mean)
      cat("\nBlock Proportions:\n")
      print(data_sbm$blockProp)
      cat("\nStored Model:\n")
      print(data_sbm$storedModels)
    })

  })
}



## To be copied in the UI
# mod_tab_sbm_ui("tab_sbm_1")

## To be copied in the server
# mod_tab_sbm_server("tab_sbm_1")
