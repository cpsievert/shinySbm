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
    mod_select_nb_groups_ui(ns("select_nb_groups_4"))
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



    my_sbm <- mod_select_nb_groups_server(
      "select_nb_groups_4",
      r$sbm$main_sbm,
      session
    )



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
          params = list(matrix = r$upload$Dataset()#,
                        # sbm = my_sbm()
                        ),
          envir = new.env(parent = globalenv())
        )
      }
    )



    # output_name <- paste0(
    #   tmp_dir,'\\',
    #   "rendered.",
    #   docu_type
    # )
    # tmp_dir <- tempdir(check = TRUE)
    # # tmp_dir <- gsub("\\\\","/",tempdir())
    # get_page <- function(){
    #   return(includeHTML(gsub("\\\\","/",output_name)))
    # }
    # observeEvent(input$doReport, {
    #
    #   tempReport <- file.path(tmp_dir, report_name())
    #
    #   file.copy(paste0("R\\",report_name()), tempReport, overwrite = T)
    #
    #   rmarkdown::render(tempReport,
    #     output_file = output_name,
    #     output_format = paste0(
    #       docu_type,
    #       "_document"
    #     ),
    #     params = list(matrix = r$upload$Dataset()),
    #     envir = globalenv()
    #   )
    #
    #   output$report_preview <- renderUI({
    #     get_page()
    #   })
    # })


  })
}

## To be copied in the UI
# mod_tab_report_ui("tab_report_1")

## To be copied in the server
# mod_tab_report_server("tab_report_1")
