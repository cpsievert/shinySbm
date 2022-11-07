#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  fluidPage(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    h1("Stochastic Block Model with the sbm package"),
    mainPanel(
      tabsetPanel(
        id = 'main_tab',
        type = 'pills',
        tabPanel("Data uploading", value = 'tab_upload',
                 #IMPORTATION
                 # Probleme : dev les moyens de lecture du tableau comme dans blockmodelingGUI
                 # introduction des covariables
                 sidebarLayout(
                   sidebarPanel(width = 8,
                     radioButtons("whichData", "Which data do you want to use ?",
                                  choices = list("My own data" = "importData",
                                                 "SBM exemple" = "sbmData"),
                                  inline = T, selected = character(0)),
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

                   mainPanel(
                     fluidRow(
                       radioButtons("whichsep", "What kind of separator should I use ?",
                                    choices = list("tabulation" = "tab",
                                                   "comma" = "comma",
                                                   "semi-colon" = "semicolon"),
                                    inline = T, selected = character(0))
                     ),
                     fluidRow(
                       verbatimTextOutput("summary")
                     )
                   )
                 )
        ),

        tabPanel("Raw Data",value = 'tab_show',
                 # RAW DATA SHOW
                 sidebarLayout(
                   sidebarPanel(width = 5,
                     radioButtons("whichshow", "Type of visualisation",
                                  choices = list("Print" = "print",
                                                 "Plot" = "simpleplot",
                                                 "Plot (nodes names)" = "namedplot"),
                                  selected = character(0)),
                     conditionalPanel(
                       condition = "input.whichshow == 'simpleplot'",
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

                   mainPanel(
                     DT::dataTableOutput("matrixprint"),
                     plotOutput("matrixplot"),
                     plotOutput("matrixplot2")
                   )
                 )
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
