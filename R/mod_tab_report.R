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
          "Français" = "_fr.Rmd",
          "English" = "_en.Rmd"
        ),
        selected = "_fr.Rmd",
        inline = T
      ),
      actionButton(ns("doReport"),label = 'Compile Report')
    ),
    shinydashboard::box(
      title = "Report Preview", solidHeader = T,
      status = "info", collapsible = T,
      uiOutput(ns("report_preview"))
    ),
    verbatimTextOutput(ns('tests')),
    mod_select_nb_groups_ui(ns("select_nb_groups_4")),
    uiOutput(ns("namesGroups"))
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


    observeEvent(input$doReport, {
      params <- list(matrix = r$upload$Dataset())

      report_name <- paste0(
        "summary_template",
        input$language
      )
      tmp_dir <- gsub("\\\\","/",tempdir())

      docu_type <- "html"
      output_name <- paste0(
        tmp_dir,'/',
        "rendered.",
        docu_type
      )

      tempReport <- file.path(
        tmp_dir,
        report_name
      )
      file.copy(paste0("R/",report_name), tempReport, overwrite = T)


      # rmarkdown::render(tempReport,
      #   output_file = output_name,
      #   output_format = paste0(
      #     docu_type,
      #     "_document"
      #   ),
      #   params = params,
      #   envir = globalenv()
      # )

      # getPage <- function(){
      #   return(includeHTML(output_name))
      # }
      # output$report_preview <- renderUI({ getPage()})
      output$tests <- renderPrint({
        print(tmp_dir)
        print(dir(tmp_dir))
      })
    })






    group_of_name <- reactive({
      getGroupNames(my_sbm(), r$sbm$Dataset())
    })

    output$trythis <- renderPrint({
      print(group_of_name()$liste)
    })

    # print(parent_session$input$`tab_sbm_1-runSbm`)

    # observe({
    #   mod_show_group_names_server("show_group_names",group_of_name()$listed_groups,1)
    #   })

    output$namesGroups <- renderUI({
      tagList(
        conditionalPanel(
          condition = "output.sbmRan == 'YES'", ns = ns_tab_upload,
          shinydashboard::box(
            title = "Groups", solidHeader = T,
            status = "info", collapsible = T, width = 9,
            verbatimTextOutput(ns("trythis"))
          )
        )
      )
    })
  })
}

## To be copied in the UI
# mod_tab_report_ui("tab_report_1")

## To be copied in the server
# mod_tab_report_server("tab_report_1")
