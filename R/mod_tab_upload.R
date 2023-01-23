#' tab_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      shinydashboard::box(
        title = "Data Selector", solidHeader = T,
        status = "info", width = 4,
        radioButtons("whichData", "Which data do you want to use ?",
          choices = list(
            "My own data" = "importData",
            "SBM exemple" = "sbmData"
          ),
          inline = T, selected = "importData"
        ),
        conditionalPanel(
          condition = "input.whichData == 'sbmData'",
          radioButtons("dataBase", "Which network ?",
            choices = list(
              "Fungus & Trees" = "fungus_tree",
              "Trees & Trees" = "tree_tree"
            ),
            selected = character(0)
          )
        ),
        conditionalPanel(
          condition = "input.whichData == 'importData'",
          fileInput("mainDataFile",
            label = "Choose the file containing your adjency matrix",
            buttonLabel = "Browse...",
            placeholder = "No file selected",
            multiple = F,
            accept = c("text/plain", ".csv", ".tab", "xls", "xlsx")
          )
        ),
        h6("* should be a adjadency matrix"),
        hr(),
        actionButton(inputId = "mainDataUploader", label = strong("Matrix Uploader"))
      ),
      conditionalPanel(
        condition = "input.whichData == 'importData'",
        shinydashboard::box(
          title = "Reading Parameters", solidHeader = T,
          status = "info", width = 3,
          radioButtons("whichSep", "What kind of separator should I use ?",
            choices = list(
              "semicolon" = ";",
              "tabulation" = "|",
              "comma" = ",",
              "others" = "others"
            )
          ),
          conditionalPanel(
            condition = "input.whichSep == 'others'",
            textInput("whichSep_other",
              label = "Write your sep character :",
              value = NULL
            )
          ),
          checkboxInput("headercol", "1st row is Columns names", value = T),
          checkboxInput("headerrow", "1st column is Rows names", value = T)
        )
      ),
      shinydashboard::box(
        title = "Network Setup", solidHeader = T,
        status = "info", width = 5,
        radioButtons("networkType", "What kind of network it is ?",
          choices = list("Bipartite" = "bipartite", "Unipartite" = "unipartite"),
          inline = T
        ),
        conditionalPanel(
          condition = "input.networkType == 'bipartite'",
          textInput("rowLabel",
            label = "Specify what are the nodes in row",
            value = NULL
          )
        ),
        conditionalPanel(
          condition = "input.networkType == 'bipartite'",
          textInput("colLabel",
            label = "Specify what are for nodes in col",
            value = NULL
          )
        ),
        conditionalPanel(
          condition = "input.networkType == 'unipartite'",
          textInput("nodLabel",
            label = "Specify what are the nodes",
            value = NULL
          )
        )
      )
    ),
    fluidRow()
  )
}

#' tab_upload Server Functions
#'
#' @noRd
mod_tab_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_tab_upload_ui("tab_upload_1")

## To be copied in the server
# mod_tab_upload_server("tab_upload_1")
