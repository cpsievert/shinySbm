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
            "SBM exemples" = "sbmData"
          ),
          inline = T, selected = "importData"
        ),
        conditionalPanel(
          condition = "input.whichData == 'importData'", ns = ns,
          radioButtons(ns("dataType"), "Select the nature of your Data",
            choices = list(
              "Adjacency or Incidence Matrix" = "matrix",
              "List of node pairs" = "list"
            ),
            inline = T, selected = "matrix"
          )
        ),
        conditionalPanel(
          condition = "input.whichData == 'sbmData'", ns = ns,
          radioButtons(ns("dataBase"), "Which network ?",
            choices = list(
              "Bipartite : Fungus & Trees" = "fungus_tree",
              "Unipartite : Trees & Trees" = "tree_tree"
            ),
            selected = character(0)
          )
        ),
        conditionalPanel(
          condition = "input.whichData == 'importData'", ns = ns,
          hr(),
          conditionalPanel(
            condition = "input.dataType == 'matrix'", ns = ns,
            tags$div(
              tags$strong("Information :"), tags$br(),
              " - It should be a ", tags$strong("adjacency or incidence"), " matrix", tags$br(),
              " - ", tags$strong("Bipartite network :"), " nodes in rows and columns can be differents", tags$br(),
              " - ", tags$strong("Unipartite network :"), " nodes in rows and columns are the same (order and names)", tags$br(),
              " - The connection is the value between one node in row and one in column", tags$br(),
              " - Values can be : 0/1, integers or decimals",
            )
          ),
          conditionalPanel(
            condition = "input.dataType == 'list'", ns = ns,
            tags$div(
              tags$strong("Information :"), tags$br(),
              " - It should be a table of ", tags$strong("two columns"), " each row specify two nodes that are connected", tags$br(),
              " - If connections are quantified a ", tags$strong("third column (numerical)"), " can be associated", tags$br(),
              " - For oriented network ", tags$strong("FROM"), " column should be the first and the ", tags$strong("TO"), " the second one"
            )
          ),
          br(),
          fileInput(ns("mainDataFile"),
            label = "Choose the file containing your data",
            buttonLabel = "Browse...",
            placeholder = "No file selected",
            multiple = F,
            accept = c("text/plain", ".csv", ".tab", "xls", "xlsx")
          ),
          hr(),
          radioButtons(ns("networkType"),
            "What kind of network it is ?",
            choices = list("Bipartite" = "bipartite", "Unipartite" = "unipartite"),
            inline = T,
            selected = "bipartite"
          ),
          conditionalPanel(
            condition = "input.dataType == 'list' & input.networkType == 'unipartite'", ns = ns,
            radioButtons(ns("orientation"), "Are connections oriented ?",
              choices = list(
                "Yes" = T,
                "No" = F
              ),
              inline = TRUE,
              selected = F
            )
          )
        ),
        hr(),
        div(
          style = "display:inline-block; float:right",
          actionButton(ns("matrixBuilder"), label = strong("Matrix Builder"))
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
          checkboxInput(ns("headercol"), "1st row is Column names", value = T),
          checkboxInput(ns("headerrow"), "1st column is Row names", value = F)
        )
      ),
      shinydashboard::box(
        title = "Network Setup", solidHeader = T,
        status = "info", width = 5,
        conditionalPanel(
          condition = "input.networkType == 'bipartite'", ns = ns,
          textInput(ns("rowLabel"),
            label = "Specify what are the nodes in row",
            value = NULL
          ),
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
        column(
          7,
          strong("Summary:"),
          mod_help_to_import_ui(ns("help_to_import_1")),
          mod_importation_error_ui(ns("error_1")),
          verbatimTextOutput(ns("summaryDataImport"))
        ),
        column(
          5,
          strong("Importation Code:"),
          verbatimTextOutput(ns("uploadCode"))
        )
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
    output$sbmRan <- renderText({
      if (session$userData$vars$sbm$runSbm != 0) {
        "YES"
      } else {
        "NO"
      }
    })
    outputOptions(output, "sbmRan", suspendWhenHidden = FALSE)

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


    ## Selected data path or name of exemples one
    datasetSelected <- eventReactive(c(
      input$whichData, input$dataBase,
      input$mainDataFile$datapath,
      sep(), input$headerrow,
      input$headercol
    ), {
      if (input$whichData == "importData") {
        validate(
          need(input$mainDataFile$datapath, "")
        )
        try_data <- read.table(file = input$mainDataFile$datapath, sep = sep(), header = input$headercol)
        if (!any(duplicated(try_data[[1]])) & input$headerrow) {
          data <- read.table(
            file = input$mainDataFile$datapath, sep = sep(),
            row.names = 1, header = input$headercol
          )
        } else {
          data <- read.table(file = input$mainDataFile$datapath, sep = sep(), header = input$headercol)
        }
      } else {
        validate(
          need(input$dataBase, "")
        )
        data <- switch(input$dataBase,
          "fungus_tree" = sbm::fungusTreeNetwork$fungus_tree %>%
            `colnames<-`(sbm::fungusTreeNetwork$tree_names) %>%
            `rownames<-`(sbm::fungusTreeNetwork$fungus_names),
          "tree_tree" = sbm::fungusTreeNetwork$tree_tree %>%
            `colnames<-`(sbm::fungusTreeNetwork$tree_names) %>%
            `rownames<-`(sbm::fungusTreeNetwork$tree_names)
        ) %>% as.data.frame()
      }
      return(data)
    })



    # reactive network adjacency matrix ("sbmMatrix" Class)
    datasetUploaded <- eventReactive(input$matrixBuilder, {
      validate(
        need(datasetSelected(), "Please select a data set")
      )
      if (input$whichData == "sbmData") {
        sbmMat <- buildSbmMatrix(datasetSelected())
      } else {
        if (input$dataType == "matrix") {
          sbmMat <- buildSbmMatrix(datasetSelected())
        } else {
          Mat <- edges_to_adjacency(datasetSelected(),
            type = input$networkType,
            oriented = as.logical(input$orientation)
          )
          sbmMat <- buildSbmMatrix(Mat)
        }
        sbmMat
      }
    })

    #  update buttons when upload a new sbmMatrix
    observeEvent(datasetUploaded(), {
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

    ## For mod importation error to get parameters
    inputs <- reactiveValues(
      whichData = NULL,
      dataType = NULL,
      headerrow = NULL,
      headercol = NULL,
      orientation = NULL,
      networkType = NULL
    )
    observe({
      inputs$whichData <- input$whichData
      inputs$dataType <- input$dataType
      inputs$headercol <- input$headercol
      inputs$headerrow <- input$headerrow
      inputs$orientation <- input$orientation
      inputs$networkType <- input$networkType
    })


    mod_help_to_import_server("help_to_import_1",
      rawData = datasetSelected,
      input_upload = inputs
    )


    # show simportation summary
    last_updated_data <- reactiveValues(v = NULL)
    observe({
      datasetSelected()
      mod_importation_error_server("error_1")
      last_updated_data$v <- 1
    })
    observe({
      datasetUploaded()
      mod_importation_error_server("error_1", datasetUploaded)
      last_updated_data$v <- 2
    })
    output$summaryDataImport <- renderPrint({
      validate(
        need(datasetSelected(), "Please select a Data Set")
      )
      if (!is.null(last_updated_data$v)) {
        if (last_updated_data$v == 1) {
          # do_drop <- input$dataType == "matrix" | input$whichData == "sbmData"
          show_table(datasetSelected(), str_len = 10, tbl_wid = 9)
        } else {
          print(datasetUploaded())
        }
      }
    })

    output$uploadCode <- renderPrint({
      if (input$whichData == "importData") {
        validate(
          need(input$mainDataFile$name, "")
        )
        if (input$headerrow) {
          headerrow <- ", row.names = 1"
        } else {
          headerrow <- ""
        }
        cat("myNetworkMatrix <- read.table(file = '",
          input$mainDataFile$name,
          "', sep = '", sep(), "', header = ", input$headercol, headerrow, ")",
          sep = ""
        )
      } else {
        validate(
          need(input$dataBase, "")
        )
        data_path <- switch(input$dataBase,
          "fungus_tree" = "sbm::fungusTreeNetwork$fungus_tree",
          "tree_tree" = "sbm::fungusTreeNetwork$tree_tree"
        )
        cat("myNetworkMatrix <- ", data_path, sep = "")
      }
    })

    return(list(
      labels = labels,
      Dataset = datasetUploaded,
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
