#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Do not move this line
    golem_add_external_resources(),

    # Beautiful title
    navbarPage(title = "ShinySBM",
               id = 'main_tab',


               ### DATA IMPORTATION
               # Probleme : dev les moyens de lecture du tableau comme dans blockmodelingGUI
               # introduction des covariables
               tabPanel("Data uploading", value = 'tab_upload',

                        ## Selector
                        column(width = 3,
                               wellPanel(
                                 fluidRow(
                                   a("Select your network data set:"),
                                   h6("It should be a adjadency matrix."),
                                   br(),
                                   radioButtons("whichData", "Which data do you want to use ?",
                                                choices = list("My own data" = "importData",
                                                               "SBM exemple" = "sbmData"),
                                                inline = T, selected = "importData"),
                                   conditionalPanel(
                                     condition = "input.whichData == 'sbmData'",
                                     radioButtons("dataBase", "Which network ?",
                                                  choices = list("Fungus & Trees" = "fungus_tree",
                                                                 "Trees & Trees" = "tree_tree"),
                                                  selected = character(0))),
                                   conditionalPanel(
                                     condition = "input.whichData == 'importData'",
                                     fileInput("dataFile", label = "Choose the file containing your adjency matrix",
                                               buttonLabel = "Browse...",
                                               placeholder = "No file selected",
                                               multiple = F,
                                               accept = c("text/plain", ".csv",".tab","xls","xlsx"))),
                                   actionButton(inputId = "main_selector", label = "Select this file ..."))),

                               wellPanel(
                                 fluidRow(
                                   a("Select a covar:"),
                                   h6("It should be a adjadency matrix, of the same size than the network matrix. So that the covariable is on the interactions between nodes."),
                                   br(),
                                   fileInput("dataFile", label = "Choose the file containing your adjency matrix",
                                             buttonLabel = "Browse...",
                                             placeholder = "No file selected",
                                             multiple = F,
                                             accept = c("text/plain", ".csv",".tab","xls","xlsx")),
                                   actionButton(inputId = "main_selector", label = "Select this file ...")))),

                        ## Reader
                        column(width = 9,
                               wellPanel(
                                 fluidRow(
                                   radioButtons("whichsep", "What kind of separator should I use ?",
                                                choices = list("tabulation" = "|",
                                                               "coma" = ",",
                                                               "semicolon" = ";",
                                                               "Others" = "others"),
                                                selected = character(0)))),
                               wellPanel(
                                 fluidRow(
                                   verbatimTextOutput("summary"))))),


               ### DATA SHOW
               tabPanel("Table Visualisation",value = 'tab_show',
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
               ),


               ### SBM APPLICATION
               tabPanel(
                 "SBM application",value = 'tab_sbm',
                 sidebarLayout(
                   sidebarPanel(width = 3,

                   ),
                   mainPanel(width = 9,

                             )
                 )
               ),


               ### NETWORK VISUALISATION
               tabPanel(
                 "Network Visualisation",value = 'tab_network',
                 sidebarLayout(
                   sidebarPanel(width = 3,

                   ),
                   mainPanel(width = 9,

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
