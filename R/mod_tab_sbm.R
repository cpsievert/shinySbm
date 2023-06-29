#' tab_sbm UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_sbm_ui <- function(id) {
  ns <- NS(id)
  ns_tab_upload <- function(id) {
    paste0("tab_upload_1-", id)
  }
  tagList(
    column(
      width = 3,
      shinydashboard::box(
        title = "SBM settings", solidHeader = T,
        status = "info", width = 12,
        selectInput(ns("whichLaw"),
          label = "What is the density expected upon dataset ?",
          choices = list(
            "Bernoulli" = "bernoulli",
            "Poisson" = "poisson",
            "Gaussian" = "gaussian"
          ),
          selected = NULL
        ),
        uiOutput(ns("sbmButton"))
      ),
      mod_select_nb_groups_ui(ns("select_nb_groups_2"), 12),
      conditionalPanel(
        condition = "output.sbmRan == 'YES'",
        ns = ns_tab_upload,
        shinydashboard::box(
          title = "Download Tables", solidHeader = T,
          status = "info", width = 12,
          radioButtons(ns("whichTable"),
                      "Select the Table",
                      choices = list(
                        "Block proportions" = "block_proportions",
                        "Connectivity Table" = "connectivity_table",
                        "Overall summary" = "stored_models"
                      ),
                      selected = "block_proportions"
          ),
          downloadButton(ns("downloadTable"),
                         label = " Save Table",
                         icon = icon("download", lib = "font-awesome")
          )
        )
      )
    ),
    column(
      width = 9,
        shinydashboard::box(
          title = "SBM outputs", solidHeader = T,
          status = "info", width = 12,
          strong("SBM code:"),
          verbatimTextOutput(ns("sbmCode")),
          mod_help_to_import_ui(ns("error_2")),
          conditionalPanel(
            condition = "output.sbmRan == 'YES'",
            ns = ns_tab_upload,
            hr(),
            strong("SBM summary:"),
            uiOutput(ns("ftsummary"))
          )
        )
      )
    )
}

#' tab_sbm Server Functions
#'
#' @noRd
mod_tab_sbm_server <- function(id, r, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$sbmButton <- renderUI({
      if (!is.null(r$upload$Dataset())) {
        tagList(
          hr(),
          div(
            style = "display:inline-block; float:right",
            actionButton(ns("runSbm"), strong("Run SBM"))
          )
        )
      }
    })


    Dataset <- eventReactive(c(r$upload$Dataset(), input$whichLaw), {
      data <- r$upload$Dataset()
      data$law <- input$whichLaw
      data
    })

    output$sbmCode <- renderText({
      importSbm <- switch(r$upload$networkType(),
        "unipartite" = paste0(
          "mySbmModel <- sbm::estimateSimpleSBM(netMat = myNetworkMatrix, model = ",
          Dataset()$law, ", estimOptions = list(verbosity = 1))"
        ),
        "bipartite" = paste0(
          "mySbmModel <- sbm::estimateBipartiteSBM(netMat = myNetworkMatrix, model = '",
          Dataset()$law, "', estimOptions = list(verbosity = 1))"
        )
      )
      if(sum(my_sbm_main()$nbBlocks) != sum(my_sbm()$nbBlocks)){
        changeGroup <- paste0("\nindex <- which(mySbmModel$storedModels['nbBlocks'] == ",
               sum(my_sbm()$nbBlocks),")\n",
               "mySbmModel$setModel(index)")
      }else{
        changeGroup <- ''
      }
      paste0(importSbm,changeGroup)
    })

    mod_help_to_import_server("error_2", sbmData = Dataset)

    my_sbm_main <- eventReactive(input$runSbm, {
      session$userData$vars$sbm$runSbm <- input$runSbm
      data_res <- withProgress(message = "SBM is Running", {
        switch(r$upload$networkType(),
          "unipartite" = sbm::estimateSimpleSBM(
            netMat = as.matrix(Dataset()),
            model = Dataset()$law, estimOptions = list(verbosity = 3, plot = F)
          ),
          "bipartite" = sbm::estimateBipartiteSBM(
            netMat = as.matrix(Dataset()),
            model = Dataset()$law, estimOptions = list(verbosity = 3, plot = F)
          )
        )
      })
      shinyalert::shinyalert(
        title = "SBM",
        text = "Calculation Done !",
        size = "xs",
        closeOnClickOutside = TRUE,
        type = "success",
        confirmButtonText = "OK"
      )
      return(data_res)
    })



    observeEvent(my_sbm_main(), {
      updateRadioButtons(parent_session, "tab_show_1-whichMatrix",
        "Select Ploted Matrix",
        choices = list(
          "Raw Matrix" = "raw",
          "Reordered Matrix" = "ordered",
          "Expected Matrix" = "expected"
        ),
        selected = "ordered"
      )
      updateRadioButtons(parent_session, "tab_network_1-whichNetwork",
        "Select Ploted Network",
        choices = list(
          "Raw Network" = "raw",
          "Grouped Network" = "grouped"
        ),
        selected = "grouped"
      )
    })


    my_sbm <- mod_select_nb_groups_server(
      "select_nb_groups_2",
      my_sbm_main,
      r$upload$labels
    )


    observeEvent(c(my_sbm(),r$upload$labels()), {
      data_sbm <- my_sbm()$clone()

      ## Exemples values
      first_par <- paste0("Tables 1 & 2 are the block description for the selected SBM. This model has an Entropy of ",
                        round(data_sbm$entropy,2),
                        ". The higher is the entropy, the less there is confusion inblock attribution.")

      example_lab <- r$upload$labels()[["row"]]
      example_connect <- round(data_sbm$connectParam$mean[1,1],2)
      law <- stringr::str_to_title(data_sbm$modelName)
      if(law == "Gaussian"){
        example_param <- paste0("(mu = ",example_connect,", sigma² = ",round(data_sbm$connectParam$var,2),").")
      }else if(law == "Poisson"){
        example_param <- paste0("(lambda = ",example_connect,").")
      }else{
        example_param <- paste0("(p = ",example_connect,").")
      }

      if(is.bipartite(data_sbm)){
        example_prop <- round_prop(data_sbm$blockProp$row,2)[[1]]
        connect_sentance <- paste0('a ',r$upload$labels()[["row"]], ' in block 1 and a ',r$upload$labels()[["col"]],' in block 1')
      }else{
        example_prop <- round_prop(data_sbm$blockProp,2)[[1]]
        connect_sentance <- 'two nodes in block 1'
      }

      second_par <- paste0("Table 1 gives blocks proportions, it's an indication of relative block sizes. If you take a random ",
                           example_lab,", it has a probability of ",
                           example_prop, " to belong to the first ",example_lab,
                           " group.")
      third_par <- paste0("Table 2 gives the connectivity values, it's an indication of block connection. Each values is a parameter of a ",
                          law ," law. We can simulate the connection between ",
                          connect_sentance," with a ",law ," law ",example_param)




      output$ftsummary <- renderUI({
        tagList(
          tags$div(tags$br(),
            first_par,
            second_par,
            third_par,
            tags$br(),
            tags$br()
          ),
          fluidRow(
            column(4,
                   flexBlockProp(data_sbm,r$upload$labels()) %>%
                     flextable::htmltools_value()),
            column(8,
                   flexConnect(data_sbm,r$upload$labels()) %>%
                     flextable::htmltools_value())
            ),
          tags$div(tags$br(),
                   "All stored models are in Table 3, the",
                   HTML("<font color=red>red line</font>"),
                   "is the best model based on ICL criteria. The",
                   HTML('<mark color=\"#FFA500\">orange line</mark>'),
                   "is the selected model.",
                   tags$br(),
                   tags$br()
          ),
          flexStoredModels(data_sbm,r$upload$labels()) %>%
            flextable::htmltools_value()
        )
      })
    })


    output$downloadTable <- downloadHandler(
      filename = eventReactive(c(my_sbm(),input$whichTable),{
          add_group <- paste0('_',sum(my_sbm()$nbBlocks),'_blocks')
        return(paste0(input$whichTable,add_group,'.png'))
      }),
      content = function(file){
        data_sbm <- my_sbm()$clone()
        if(input$whichTable == "block_proportions"){
          ft <- flexBlockProp(data_sbm,r$upload$labels())
        }else if(input$whichTable == "connectivity_table"){
          ft <- flexConnect(data_sbm,r$upload$labels())
        }else{
          ft <- flexStoredModels(data_sbm,r$upload$labels())
        }
        flextable::save_as_image(ft,path = file)
      }
    )






    return(list(
      Dataset = Dataset,
      main_sbm = my_sbm_main,
      runSbm = reactive({
        input$runSbm
      })
    ))
  })
}



## To be copied in the UI
# mod_tab_sbm_ui("tab_sbm_1")

## To be copied in the server
# mod_tab_sbm_server("tab_sbm_1")
