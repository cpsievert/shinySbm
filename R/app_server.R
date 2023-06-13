#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  session$userData$vars <- reactiveValues(
    tab = reactive({input$tab}),
    sbm = list(NbBlocks = 4,
               runSbm = 0)
  )

  r <- reactiveValues(
    upload = reactiveValues(),
    sbm = reactiveValues(),
    show = reactiveValues()
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
  mod_tab_network_server("tab_network_1", r)

  mod_tab_report_server("tab_report_1", r)

  mod_tab_extraction_server("tab_extraction_1",r)

  mod_tab_about_us_server("tab_about_us_1")


  # shut down the app when it's closes on the browser
  session$onSessionEnded(function() {
    stopApp()
  })
}
