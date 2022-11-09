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
               # no conditionalpanel work !!!
               tabPanel("Data uploading", value = 'tab_upload',

                        ## Selector
                        column(width = 3,
                               fluidRow(a(strong("Network data set selector:"))),
                               fluidRow(
                                 wellPanel(
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
                                     fileInput("mainDataFile", label = "Choose the file containing your adjency matrix",
                                               buttonLabel = "Browse...",
                                               placeholder = "No file selected",
                                               multiple = F,
                                               accept = c("text/plain", ".csv",".tab","xls","xlsx")),
                                     actionButton(inputId = "mainDataSelector", label = "Select file")),
                                   h6("* should be a adjadency matrix"))),


                                 conditionalPanel(
                                   condition = "input.mainDataSelector",
                                   fluidRow(a(strong("Covariable data set selector:"))),
                                   fluidRow(
                                     wellPanel(fileInput("covarDataFile", label = "Choose the file containing your covariable",
                                                         buttonLabel = "Browse...",
                                                         placeholder = "No file selected",
                                                         multiple = F,
                                                         accept = c("text/plain", ".csv",".tab","xls","xlsx")),
                                               actionButton(inputId = "covarDataSelector", label = "Select file"),
                                               h6("* should be a adjadency matrix"),
                                               h6("* same size than the network matrix"),
                                               h6("* covariable on the interactions between nodes"))))),
                        column(width = 1),
                        ## Reader
                        column(width = 7,
                               fluidRow(a(strong("Data Reader"))),
                               fluidRow(
                                 column(width = 5,
                                        wellPanel(
                                          radioButtons("whichSep", "What kind of separator should I use ?",
                                                       choices = list("semicolon" = ";",
                                                                      "tabulation" = "|",
                                                                      "coma" = ",",
                                                                      "others" = "others"),
                                                       selected = "semicolon"),
                                          conditionalPanel(
                                            condition = "input.whichSep == 'others'",
                                            textInput("whichSep_other",
                                                      label = "Write your sep character :",
                                                      value = NULL)))),
                                 column(width = 5,
                                        wellPanel(
                                          radioButtons("networkType", "What kind of network it is ?",
                                                       choices = list("Unipartite" = "unipartite",
                                                                      "Bipartite" = "bipartite"),
                                                       inline = T ,selected = character(0)))),
                                 column(width = 2,
                                        wellPanel(
                                          strong("Matrix Uploader"),
                                          conditionalPanel(
                                            condition = "input.mainDataSelector",
                                            actionButton(inputId = "mainDataUploader", label = "Upload Matrix")),
                                          conditionalPanel(
                                            condition = "input.covarDataSelector",
                                            actionButton(inputId = "covarDataUploader", label = "Upload covar"))))),
                               fluidRow(
                                 a(strong("Importation Information")),
                                 verbatimTextOutput("summaryDataImport"))),
                        column(width = 1)),

               ### DATA SHOW
               tabPanel("Table Visualisation",value = 'tab_show',

                        column(width = 3,
                               a(strong("Visualisation settings")),
                               wellPanel(
                                 radioButtons("whichShow", "Type of visualisation",
                                              choices = list("Print" = "print",
                                                             "Plot" = "plot"),
                                              inline  = T, selected = 'print'),
                                 conditionalPanel(
                                   condition = "TRUE",
                                   radioButtons("whichRawSbm", "Select Ploted Matrix",
                                                choices = list("Raw Matrix" = "raw",
                                                               "Reordered Matrix" = "reordered"),
                                                inline = T, selected = 'raw')),
                                 conditionalPanel(
                                   condition = "TRUE",
                                   sliderInput(inputId = "NbGroup",
                                               label = "Select the number of group:",
                                               value = 4, min = 1, max = 6,step=1),
                                   plotOutput("showILC")),
                                 conditionalPanel(
                                   condition = "input.whichShow == 'print'",
                                   wellPanel(
                                     fluidRow(
                                       downloadButton("downloadTable", "Download Table")))),
                                 conditionalPanel(
                                   condition = "input.whichShow == 'plot' || input.whichShow == 'plotSimp'",
                                   wellPanel(
                                     fluidRow(
                                       textInput("rowLabel",
                                                 label = "Specify the label for nodes in row",
                                                 value = NULL),
                                       textInput("colLabel",
                                                 label = "Specify the label for nodes in col",
                                                 value = NULL),
                                       downloadButton("downloadPlot", "Download Plot")))))),

                        column(width = 1),

                        column(width = 6,
                               a(strong("Plot screen")),
                               conditionalPanel(
                                 condition = "input.whichShow == 'print'",
                                 DT::dataTableOutput("matrixPrint")),
                               conditionalPanel(
                                 condition = "input.whichShow == 'plot' || input.whichShow == 'plotSimp'",
                                 imageOutput("matrixPlot")))),


               ### SBM APPLICATION
               tabPanel(
                 "SBM application",value = 'tab_sbm',

                 sidebarLayout(
                   sidebarPanel(width = 3,

                                ),

                   mainPanel(width = 9
                             ))),


               ### NETWORK VISUALISATION
               tabPanel(
                 "Network Visualisation",value = 'tab_network',

                 sidebarLayout(
                   sidebarPanel(width = 3
                                ),

                   mainPanel(width = 9
                             )))))}

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
