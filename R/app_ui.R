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
                                               accept = c("text/plain", ".csv",".tab","xls","xlsx"))),
                                   h6("* should be a adjadency matrix"))),


                                 conditionalPanel(
                                   condition = "input.mainDataUploader",
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
                        ## Reader
                        column(width = 9,
                               fluidRow(a(strong("Data Reader"))),
                               fluidRow(
                                 column(width = 5,
                                        wellPanel(
                                          radioButtons("whichSep", "What kind of separator should I use ?",
                                                       choices = list("semicolon" = ";",
                                                                      "tabulation" = "|",
                                                                      "comma" = ",",
                                                                      "others" = "others")),
                                          conditionalPanel(
                                            condition = "input.whichSep == 'others'",
                                            textInput("whichSep_other",
                                                      label = "Write your sep character :",
                                                      value = NULL)),
                                          checkboxInput('headercol','1st row is Columns names', value = T),
                                          checkboxInput('headerrow','1st column is Rows names',value = T))),
                                 column(width = 4,
                                        wellPanel(
                                          radioButtons("networkType", "What kind of network it is ?",
                                                       choices = list("Bipartite" = "bipartite","Unipartite" = "unipartite"),
                                                       inline = T),

                                                       wellPanel(
                                                         conditionalPanel(
                                                           condition = "input.networkType == 'bipartite'",
                                                           textInput("rowLabel",
                                                                     label = "Specify what are the nodes in row",
                                                                     value = NULL)),
                                                         conditionalPanel(
                                                           condition = "input.networkType == 'bipartite'",
                                                           textInput("colLabel",
                                                                     label = "Specify what are for nodes in col",
                                                                     value = NULL)),
                                                         conditionalPanel(
                                                           condition = "input.networkType == 'unipartite'",
                                                           textInput("nodLabel",
                                                                     label = "Specify what are for nodes",
                                                                     value = NULL))))),

                                 column(width = 3,
                                        wellPanel(
                                          strong("Matrix Uploader"),
                                          actionButton(inputId = "mainDataUploader", label = "Upload Matrix"),
                                          conditionalPanel(
                                            condition = "input.covarDataSelector",
                                            actionButton(inputId = "covarDataUploader", label = "Upload covar"))))),
                               fluidRow(
                                 a(strong("Importation Information")),
                                 column(width = 12,
                                        wellPanel(
                                          verbatimTextOutput("warningDataImport1"),
                                          tags$head(tags$style("#warningDataImport1{color: red}")),
                                          verbatimTextOutput("summaryDataImport")))))),

               ### DATA SHOW
               tabPanel("Table Visualisation",value = 'tab_show',

                        column(width = 3,
                               a(strong("Visualisation settings")),
                               wellPanel(
                                   radioButtons("whichShow", "Type of visualisation",
                                                choices = list("Print a table" = 'print',
                                                               "Plot a matrix" = 'plot'),
                                                inline  = T),
                                   radioButtons("whichRawSbmMatrix", "Select Ploted Matrix",
                                                choices = list("Raw Matrix" = "raw")),
                                   conditionalPanel(
                                     condition = "input.runSbm",
                                     plotOutput("showILC1")),
                                   br(),
                                 downloadButton("downloadPlot"))),

                        column(width = 1),
                        column(width = 6,
                               a(strong("Plot screen")),
                               conditionalPanel(
                                 condition = "input.whichShow == 'print'",
                                 DT::dataTableOutput("matrixPrint")),
                               conditionalPanel(
                                 condition = "input.whichShow != 'print'",
                                 imageOutput("matrixPlot")))),


               ### SBM APPLICATION
               tabPanel(
                 "SBM application",value = 'tab_sbm',

                 column(width = 3,
                        a(strong("SBM settings")),
                        wellPanel(
                          selectInput("whichLaw",
                                      label = "What is the density expected upon dataset ?",
                                      choices = list("Bernoulli" = "bernoulli",
                                                     "Poisson" = "poisson",
                                                     "Gaussian" = "gaussian"),
                                      selected = NULL),
                          selectInput("whichCovar",
                                      label = "Add covariable to the SBM:",
                                      choices = list("None" = "NULL")),
                          actionButton(inputId = "runSbm", "Run SBM"))),
                 column(width = 9,
                        a(strong("SBM ouputs")),
                        wellPanel(
                          fluidRow(
                            column(width = 8,
                                   strong("SBM code:"),
                                   verbatimTextOutput("sbmCode"),
                                   verbatimTextOutput("warningDataImport2"),
                                   tags$head(tags$style("#warningDataImport2{color: red}")),
                                   hr(),
                                   strong("SBM summary:"),
                                   verbatimTextOutput("sbmSummarySelect"),
                                   tags$head(tags$style("#sbmSummarySelect{font-weight: bold}")),
                                   verbatimTextOutput("sbmSummary")),

                            column(width = 4,
                                   numericInput(inputId = "Nbblocks",
                                               label = "Select the total number of blocks:",
                                               value = 4, min = 1, max = 6,step=1),
                                   plotOutput("showILC2"))
                          )))),


               ### NETWORK VISUALISATION
               tabPanel(
                 "Network Visualisation",value = 'tab_network',

                 column(width = 3,
                        a(strong("Visualisation settings")),
                        wellPanel(
                          radioButtons("whichRawSbmNetwork", "Select Ploted Network:",
                                       choices = list("Raw Network" = "raw"),
                                       inline = T),
                          conditionalPanel(
                            condition = "input.runSbm",
                            plotOutput("showILC3")),
                          br(),
                          downloadButton("downloadNetworkPlot"))),

                 column(width = 1),
                 column(width = 8,
                        a(strong("Network Visual")),
                        plotOutput("networkPlot")))))
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
