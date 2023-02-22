#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # session$userData$vars <- reactiveValues(NbBlocks = 4,
  #                                         show_block = F)

  r <- reactiveValues(
    upload = reactiveValues(),
    show = reactiveValues(),
    sbm = reactiveValues(),
    network = reactiveValues(),
    clustering = reactiveValues()
  )

  ## Importing the data set
  r$upload <- mod_tab_upload_server("tab_upload_1", r,
                          parent_session = session
    )


  ## SBM part
  r$sbm <-  mod_tab_sbm_server("tab_sbm_1", r,
                       parent_session = session
                       )

  ## Visualisation part
  r$show <- mod_tab_show_server("tab_show_1", r)


  ## Network visualisation part
  r$network <- mod_tab_network_server("tab_network_1", r)

  r$clustering <- mod_tab_clustering_server("tab_clustering_1", r)

  # shut down the app when it's closes on the browser
  session$onSessionEnded(function() {
    stopApp()
  })
}
