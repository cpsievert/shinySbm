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


    output$params <- renderPrint({
      print(reactiveValuesToList(params))
    })

    my_sbm <- mod_select_nb_groups_server(
      "select_nb_groups_4",
      r$sbm$main_sbm,
      session
    )

    ## Parameters from tab_show

    params <- reactiveValues(upload = NA,
                             sbm = NA,
                             options = NA)


    observeEvent(purrr::map(r$upload,~.x()),{
      params$upload  <- purrr::map(r$upload,~.x())
    })

    observeEvent(my_sbm(),{
      params$sbm <- my_sbm()
    })

    observeEvent(purrr::map(r$show,~.x()),{
      params$options  <- purrr::map(r$show,~.x())
    })



    output$downReport <- downloadHandler(
      filename = reactive({paste0(input$fileName,'.',input$fileType)}),
      content = function(file) {
        file_names <- c("summary_template","child_imported","child_sbm")
        visual_names <- c("child_imported_visual.Rmd","child_sbm_visual.Rmd")
        rmd_names <- purrr::map_chr(file_names,~paste0(.x,input$language))
        all_files <- c(rmd_names,visual_names)
        file_paths <- purrr::map_chr(all_files,
                                     ~system.file("rmd",.x, package = "shinySbm"))
        tempReports <- purrr::map_chr(all_files,
                                      ~file.path(tempdir(), .x,fsep = '/'))
        purrr::map2(file_paths,tempReports,~file.copy(.x,.y, overwrite = TRUE))

        rmarkdown::render(
          input = tempReports[[1]],
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
