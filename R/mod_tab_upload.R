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
          condition = "input.whichData == 'importData'", ns = ns,
          radioButtons(ns("dataType"), "Select the nature of your Data",
                       choices = list(
                         "Adjacency or Incidence Matrix" = "matrix",
                         "List of pair of nodes" = "list"
                       ),
                       inline = T, selected = "matrix"
          )
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
          condition = "input.whichData == 'importData' & input.dataType == 'matrix'", ns = ns,
          fileInput(ns("mainDataFile"),
            label = "Choose the file containing your adjacency matrix",
            buttonLabel = "Browse...",
            placeholder = "No file selected",
            multiple = F,
            accept = c("text/plain", ".csv", ".tab", "xls", "xlsx")
          ),
          tags$div(
            tags$strong("Information :") , tags$br(),
            " - It should be a ", tags$strong("adjacency or incidence")," matrix", tags$br(),
            " - ", tags$strong("Bipartite network :")," nodes in rows and columns can be differents", tags$br(),
            " - ", tags$strong("Unipartite network :")," nodes in rows and columns are the same (order and names)", tags$br(),
            " - The connection is the value between one node in row and one in column", tags$br(),
            " - Values can be : 0/1, integers or decimals",
          )
        ),
        conditionalPanel(
          condition = "input.whichData == 'importData' & input.dataType == 'list'", ns = ns,
          radioButtons(ns("orientation"), "Are connections oriented ?",
                       choices = list(
                         "Yes" = TRUE,
                         "No" = FALSE
                       ),
                       inline = TRUE,
                       selected = FALSE
          ),
          fileInput(ns("mainDataFileList"),
                    label = "Choose the file containing your list",
                    buttonLabel = "Browse...",
                    placeholder = "No file selected",
                    multiple = F,
                    accept = c("text/plain", ".csv", ".tab", "xls", "xlsx")
          ),
          tags$div(
            tags$strong("Information :") , tags$br(),
            " - It should be a table of ", tags$strong("two columns")," each row specify two nodes that are connected", tags$br(),
            " - If connections are quantified a ", tags$strong("third column (numerical)")," can be associated", tags$br(),
            " - For oriented network ", tags$strong("FROM")," column should be the first and the ", tags$strong("TO")," the second one"
            )
        ),
        hr(),
        conditionalPanel(
          condition = "input.whichData == 'importData' & input.dataType == 'list'", ns = ns,
          actionButton(ns("listUploader"), label = strong("Load Data")),
          div(
            style = "display:inline-block; float:right",
            conditionalPanel(
              condition = "input.listUploader", ns = ns,
              actionButton(ns("matrixBuilder"), label = strong("Matrix Builder"))
            )
          )
        ),
        conditionalPanel(
          condition = "input.whichData != 'importData' | input.dataType != 'list'", ns = ns,
          div(
            style = "display:inline-block; float:right",
            actionButton(ns("mainDataUploader"), label = strong("Matrix Uploader"))
          )
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
mod_tab_upload_server <- function(id, r, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## will be used in other modules inside conditionnal panels that should only be shown when the sbm has been run
    # reset when loading a new matrix
    output$sbmRan = renderText({
      if (session$userData$vars$sbm$runSbm != 0){"YES"}else{"NO"}
    })
    outputOptions(output, 'sbmRan', suspendWhenHidden = FALSE)

    ## Settings reactive labels for plots (nature of stuff in cols and rows)
    labels <- eventReactive(c(input$networkType, input$rowLabel, input$colLabel, input$nodLabel), {
      labels_sets <- switch(input$networkType,
        "bipartite" = list(row = input$rowLabel, col = input$colLabel),
        "unipartite" = list(row = input$nodLabel, col = input$nodLabel)
      )
      list(
        row = ifelse(labels_sets$row == "", "row", labels_sets$row),
        col = ifelse(labels_sets$col == "", "col", labels_sets$col)
      )
    })

    ## Selected data path or name of exemples one
    datasetSelected <- eventReactive(c(input$whichData, input$dataBase, input$mainDataFile$datapath), {
      if (input$whichData == "importData") {
        input$mainDataFile$datapath
      } else {
        input$dataBase
      }
    })

    # reactive separator for reading
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

    # reactive network adjacency matrix ("sbmMatrix" Class)
    datasetUploaded <- eventReactive(input$mainDataUploader, {
      validate(
        need(datasetSelected(), "Please select a data set")
      )
      if (input$whichData == "importData") {
        if(input$dataType == "matrix"){
          if (input$headerrow) {
            x <- read.table(file = datasetSelected(), sep = sep(), row.names = 1, header = input$headercol)
          } else {
            x <- read.table(file = datasetSelected(), sep = sep(), header = input$headercol)
          }
          dataset <- buildSbmMatrix(x)
        }else if(input$dataType == "list"){
          if (input$headerrow) {
            x <- read.table(file = datasetSelected(), sep = sep(), row.names = 1, header = input$headercol)
          } else {
            x <- read.table(file = datasetSelected(), sep = sep(), header = input$headercol)
          }
          adjacency_matrix <- edges_to_adjacency(x, type = input$networkType , oriented = input$orientation)
          dataset <- buildSbmMatrix(adjacency_matrix)
        }else{
          stop("input$dataType has the wrong type only 'matrix' and 'list' have been set yet")
        }
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

    #  update buttons when upload a new sbmMatrix
    observeEvent(datasetUploaded(), {
      updateRadioButtons(session, "networkType",
        "What kind of network it is ?",
        choices = list("Bipartite" = "bipartite", "Unipartite" = "unipartite"),
        inline = T,
        selected = datasetUploaded()$type
      )
      updateSelectInput(parent_session, "tab_sbm_1-whichLaw",
        label = "What is the density expected upon dataset ?",
        choices = list(
          "Bernoulli" = "bernoulli",
          "Poisson" = "poisson",
          "Gaussian" = "gaussian"
        ),
        selected = datasetUploaded()$law
      )
      updateRadioButtons(parent_session, "tab_show_1-whichMatrix",
        "Select Ploted Matrix",
        choices = list("Raw Matrix" = "raw")
      )
      updateRadioButtons(parent_session, "tab_network_1-whichNetwork",
        "Select Ploted Network",
        choices = list("Raw Network" = "raw")
      )
      updateActionButton(parent_session, "tab_sbm_1-runSbm")
      # global variable reset the runsbm variable to 0
      session$userData$vars$sbm$runSbm <- 0
    })

    # Set new network type on the current dataset but it remain the old uploaded one
    workingDataset <- eventReactive(c(datasetUploaded(), input$networkType), {
      data <- datasetUploaded()
      data$type <- input$networkType
      data
    })

    # Get the changes from Sbm page (allow the warnings to transmit when user change the law upon values)
    observedDataset <- eventReactive(r$sbm$Dataset(), {
      if (is.null(r$sbm$Dataset())) {
        return(workingDataset())
      } else {
        return(r$sbm$Dataset())
      }
    })

    mod_importation_error_server("error_1", observedDataset)

    # show simportation summary
    output$summaryDataImport <- renderPrint({
      print(observedDataset())
    })

    return(list(
      labels = labels,
      Dataset = workingDataset,
      networkType = reactive({
        input$networkType
      })
    ))
  })
}

## To be copied in the UI
# mod_tab_upload_ui("tab_upload_1")

## To be copied in the server
# mod_tab_upload_server("tab_upload_1")
