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
        radioButtons(ns("whichData"), "Which data do you want to use ?",
          choices = list(
            "My own data" = "importData",
            "SBM exemple" = "sbmData"
          ),
          inline = T, selected = "importData"
        ),
        conditionalPanel(
          condition = "input.whichData == 'sbmData'", ns = ns,
          radioButtons(ns("dataBase"), "Which network ?",
            choices = list(
              "Fungus & Trees" = "fungus_tree",
              "Trees & Trees" = "tree_tree"
            ),
            selected = character(0)
          )
        ),
        conditionalPanel(
          condition = "input.whichData == 'importData'", ns = ns,
          fileInput(ns("mainDataFile"),
            label = "Choose the file containing your adjency matrix",
            buttonLabel = "Browse...",
            placeholder = "No file selected",
            multiple = F,
            accept = c("text/plain", ".csv", ".tab", "xls", "xlsx")
          )
        ),
        h6("* should be a adjadency matrix"),
        hr(),
        div(
          style = "display:inline-block; float:right",
          actionButton(ns("mainDataUploader"), label = strong("Matrix Uploader"))
        )
      ),
      conditionalPanel(
        condition = "input.whichData == 'importData'", ns = ns,
        shinydashboard::box(
          title = "Reading Parameters", solidHeader = T,
          status = "info", width = 3,
          radioButtons(ns("whichSep"), "What kind of separator should I use ?",
            choices = list(
              "semicolon" = ";",
              "tabulation" = "|",
              "comma" = ",",
              "others" = "others"
            )
          ),
          conditionalPanel(
            condition = "input.whichSep == 'others'", ns = ns,
            textInput(ns("whichSep_other"),
              label = "Write your sep character :",
              value = NULL
            )
          ),
          checkboxInput(ns("headercol"), "1st row is Columns names", value = T),
          checkboxInput(ns("headerrow"), "1st column is Rows names", value = T)
        )
      ),
      shinydashboard::box(
        title = "Network Setup", solidHeader = T,
        status = "info", width = 5,
        radioButtons(ns("networkType"), "What kind of network it is ?",
          choices = list("Bipartite" = "bipartite", "Unipartite" = "unipartite"),
          inline = T
        ),
        conditionalPanel(
          condition = "input.networkType == 'bipartite'", ns = ns,
          textInput(ns("rowLabel"),
            label = "Specify what are the nodes in row",
            value = NULL
          )
        ),
        conditionalPanel(
          condition = "input.networkType == 'bipartite'", ns = ns,
          textInput(ns("colLabel"),
            label = "Specify what are for nodes in col",
            value = NULL
          )
        ),
        conditionalPanel(
          condition = "input.networkType == 'unipartite'", ns = ns,
          textInput(ns("nodLabel"),
            label = "Specify what are the nodes",
            value = NULL
          )
        )
      )
    ),
    fluidRow(
      shinydashboard::box(
        title = "Importation Details", solidHeader = T,
        status = "info", width = 12,
        mod_importation_error_ui(ns("error_1")),
        verbatimTextOutput(ns("summaryDataImport"))
      )
    )
  )
}



#' tab_upload Server Functions
#'
#' @noRd
mod_tab_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    labels <- eventReactive(c(input$networkType, input$rowLabel, input$colLabel, input$nodLabel), {
      switch(input$networkType,
        "bipartite" = list(row = input$rowLabel, col = input$colLabel),
        "unipartite" = list(row = input$nodLabel, col = input$nodLabel)
      )
    })

    datasetSelected <- eventReactive(c(input$whichData, input$dataBase, input$mainDataFile$datapath), {
      if (input$whichData == "importData") {
        input$mainDataFile$datapath
      } else {
        input$dataBase
      }
    })

    sep <- reactive({
      if (input$whichSep == "others") {
        if (input$whichSep_other == "") {
          ";"
        } else {
          input$whichSep_other
        }
      } else {
        input$whichSep
      }
    })

    datasetUploaded <- eventReactive(input$mainDataUploader, {
      validate(
        need(datasetSelected(), "Please select a data set")
      )
      if (input$whichData == "importData") {
        if (input$headerrow) {
          x <- read.table(file = datasetSelected(), sep = sep(), row.names = 1, header = input$headercol)
        } else {
          x <- read.table(file = datasetSelected(), sep = sep(), header = input$headercol)
        }
        dataset <- buildSbmMatrix(x)
      } else {
        dataset <- switch(datasetSelected(),
          "fungus_tree" = buildSbmMatrix(sbm::fungusTreeNetwork$fungus_tree,
            col_names = sbm::fungusTreeNetwork$tree_names,
            row_names = sbm::fungusTreeNetwork$fungus_names
          ),
          "tree_tree" = buildSbmMatrix(sbm::fungusTreeNetwork$tree_tree,
            col_names = sbm::fungusTreeNetwork$tree_names,
            row_names = sbm::fungusTreeNetwork$tree_names
          )
        )
      }
      dataset
    })

    observeEvent(datasetUploaded(), {
      updateRadioButtons(session, "networkType",
        "What kind of network it is ?",
        choices = list("Bipartite" = "bipartite", "Unipartite" = "unipartite"),
        inline = T,
        selected = datasetUploaded()$type
      )
      # updateSelectInput(session, ns("whichLaw"),
      #                   label = "What is the density expected upon dataset ?",
      #                   choices = list(
      #                     "Bernoulli" = "bernoulli",
      #                     "Poisson" = "poisson",
      #                     "Gaussian" = "gaussian"
      #                   ),
      #                   selected = datasetUploaded()$law
      # )
    })

    workingDataset <- eventReactive(c(datasetUploaded(), input$networkType, input$whichLaw), {
      data <- datasetUploaded()
      data$type <- input$networkType
      # data$law <- input$whichLaw
      data
    })

    mod_importation_error_server("error_1", workingDataset)

    output$summaryDataImport <- renderPrint({
      print(workingDataset())
    })

    return(list(
      labels = labels,
      workingDataset = workingDataset,
      networkType = reactive({input$networkType})
    ))
  })
}

## To be copied in the UI
# mod_tab_upload_ui("tab_upload_1")

## To be copied in the server
# mod_tab_upload_server("tab_upload_1")
