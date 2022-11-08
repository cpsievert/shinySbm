#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  navbarPage(title = "ShinySBM",
             id = 'main_tab',
             golem_add_external_resources(),
             tabPanel("Data uploading", value = 'tab_upload',
                      #IMPORTATION
                      # Probleme : dev les moyens de lecture du tableau comme dans blockmodelingGUI
                      # introduction des covariables
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     radioButtons("whichData", "Which data do you want to use ?",
                                                  choices = list("My own data" = "importData",
                                                                 "SBM exemple" = "sbmData"),
                                                  inline = T, selected = "importData"),
                                     conditionalPanel(
                                       condition = "input.whichData == 'sbmData'",
                                       radioButtons("dataBase", "Which network ?",
                                                    choices = list("Fungus & Trees" = "fungus_tree",
                                                                   "Trees & Trees" = "tree_tree"),
                                                    selected = character(0))
                                     ),
                                     conditionalPanel(
                                       condition = "input.whichData == 'importData'",
                                       fileInput("dataFile", label = "Choose the file containing your adjency matrix",
                                                 buttonLabel = "Browse...",
                                                 placeholder = "No file selected",
                                                 multiple = F,
                                                 accept = c("text/plain", ".csv",".tab","xls","xlsx"))
                                     )
                        ),

                        mainPanel(width = 9,
                                  fluidRow(
                                    radioButtons("whichsep", "What kind of separator should I use ?",
                                                 choices = list("tabulation" = "|",
                                                                "coma" = ",",
                                                                "semicolon" = ";",
                                                                "Others" = "others")
                                                 , selected = character(0))
                                  ),
                                  fluidRow(
                                    verbatimTextOutput("summary")
                                  )
                        )
                      )
             ),

             tabPanel("Visualisation",value = 'tab_show',
                      # RAW DATA SHOW
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     radioButtons("whichshow", "Type of visualisation",
                                                  choices = list("Print" = "print",
                                                                 "Plot" = "plot"),
                                                  selected = 'print'),
                                     conditionalPanel(
                                       condition = "input.whichshow == 'plot'",
                                       wellPanel(
                                         fluidRow(
                                           textInput("rowLabel",
                                                     label = "Specify the label for nodes in row",
                                                     value = NULL),
                                           textInput("colLabel",
                                                     label = "Specify the label for nodes in col",
                                                     value = NULL),
                                           downloadButton("downloadrawPlot", "Download Plot")
                                         )
                                       )
                                     )
                        ),

                        mainPanel(width = 9,
                                  conditionalPanel(
                                    condition = "input.whichshow == 'print'",
                                    DT::dataTableOutput("matrixprint")),
                                  conditionalPanel(
                                    condition = "input.whichshow == 'plot'",
                                    imageOutput("matrixplot"))
                        )
                      )
             )


  )

}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "shinySbm"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
