#' tab_report UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      title = "Parameters", solidHeader = T,
      status = "info", collapsible = T,
      radioButtons(ns("language"), "Select your language:",
        choices = list(
          "Francais" = "_fr.Rmd",
          "English" = "_en.Rmd"
        ),
        selected = "_fr.Rmd",
        inline = T
      ),
      textInput(ns("fileName"),
                label = "File Name",
                value = "Shiny_SBM_Report"
      ),
      radioButtons(ns("fileType"), "Select file type:",
                   choices = list(
                     "pdf" = "pdf",
                     "html" = "html"
                   ),
                   selected = "html",
                   inline = T
      ),
      downloadButton(ns("downReport"),label = 'Download Report')
    ),
    mod_select_nb_groups_ui(ns("select_nb_groups_4")),
    verbatimTextOutput(ns('params'))
  )
}

#' tab_report Server Functions
#'
#' @noRd
mod_tab_report_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    ns_tab_sbm <- function(id) {
      paste0("tab_sbm_1-", id)
    }
    ns_tab_upload <- function(id) {
      paste0("tab_upload_1-", id)
    }

    params <- reactiveValues(matrix = NA,
                             sbm = NA,
                             options = list())

    output$params <- renderPrint({
      print(reactiveValuesToList(params))
    })

    my_sbm <- mod_select_nb_groups_server(
      "select_nb_groups_4",
      r$sbm$main_sbm,
      session
    )

    observeEvent(r$upload$Dataset(),{
      params$matrix <- r$upload$Dataset()$matrix
    })

    observeEvent(my_sbm(),{
      params$sbm <- my_sbm()
    })



    output$downReport <- downloadHandler(
      filename = reactive({paste0(input$fileName,'.',input$fileType)}),
      content = function(file) {
        rmd_name <-  paste0("summary_template",input$language)
        file_path <-  system.file("rmd",rmd_name, package = "shinySbm")
        tempReport <- file.path(tempdir(), rmd_name,fsep = '\\')
        file.copy(file_path, tempReport, overwrite = TRUE)
        rmarkdown::render(
          input = tempReport,
          output_file = file,
          output_format = paste0(input$fileType, "_document"),
          params = reactiveValuesToList(params),
          envir = new.env(parent = globalenv())
        )
      }
    )

  })
}

## To be copied in the UI
# mod_tab_report_ui("tab_report_1")

## To be copied in the server
# mod_tab_report_server("tab_report_1")
