#' tab_extraction UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_extraction_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      shinydashboard::box(
      title = "Parameters", solidHeader = T,
      status = "info", collapsible = T,
      strong("Select the informations you want:"),
      fluidRow(
        column(width = 6,
               checkboxInput(ns("attribution"),label = "Group membership",value = T)),
        column(width = 6,
               checkboxInput(ns("proportion"),label = "Probability of group membership",value = T))
      ),
      hr(),
      textInput(ns("fileName"),
                label = "File Name",
                value = "Group_list"
      ),
      radioButtons(ns("fileType"), "Select file type:",
                   choices = list(
                     "csv" = "csv",
                     "xls" = "xls",
                     "txt" = "txt"
                   ),
                   selected = "csv",
                   inline = T
      ),
      uiOutput(ns("downButtons"))
    ),
    mod_select_nb_groups_ui(ns("select_nb_groups_5"))),
    shinydashboard::box(title = "Tables", solidHeader = T,
                        status = "info", collapsible = T,width = 12,
                        verbatimTextOutput(ns("test")))
  )
}

#' tab_extraction Server Functions
#'
#' @noRd
mod_tab_extraction_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    my_sbm <- mod_select_nb_groups_server(
      "select_nb_groups_5",
      r$sbm$main_sbm,
      session
    )
    to_extract <- reactive({getGroups(my_sbm(),r$upload$Dataset(),
                                      input$attribution,input$proportion,
                                      r$upload$labels())})


    output$test <- renderPrint({
      to_extract()
    })

    check_sbm <- reactiveValues(is_sbm = F)
    observeEvent(my_sbm(),{
      check_sbm$is_sbm <- T
    })

    output$downButtons <- renderUI({
      if(r$upload$networkType() == 'bipartite'){
        tagList(
          downloadButton(ns("downDataRow"),label = paste('Extract',r$upload$labels()$row,'Data')),
          downloadButton(ns("downDataCol"),label = paste('Extract',r$upload$labels()$col,'Data'))
        )
      }else{
        downloadButton(ns("downData"),label = 'Extract Data')
      }
    })

    output$downData <- downloadHandler(
      filename = eventReactive(c(my_sbm(),input$fileType,input$fileName),{
        if(check_sbm$is_sbm){
          add_group <- paste0('_',sum(my_sbm()$nbBlocks),'_groups')
        }else{
          add_group <- ''
        }
        return(paste0(input$fileName,add_group,'.',input$fileType))
      }),
      content = function(file){
        write.csv2(to_extract(), file,row.names=F)
      }
    )
    output$downDataRow <- downloadHandler(
      filename = eventReactive(c(my_sbm(),input$fileType,input$fileName),{
        if(check_sbm$is_sbm){
          add_group <- paste0('_',sum(my_sbm()$nbBlocks),'_groups')
        }else{
          add_group <- ''
        }
        return(paste0(input$fileName,'_',r$upload$labels()$row,add_group,'.',input$fileType))
      }),
      content = function(file){
        write.csv2(to_extract()$row, file,row.names=F)
      }
    )
    output$downDataCol <- downloadHandler(
      filename = eventReactive(c(my_sbm(),input$fileType,input$fileName),{
        if(check_sbm$is_sbm){
          add_group <- paste0('_',sum(my_sbm()$nbBlocks),'_groups')
        }else{
          add_group <- ''
        }
        return(paste0(input$fileName,'_',r$upload$labels()$col,add_group,'.',input$fileType))
      }),
      content = function(file){
        write.csv2(to_extract()$col, file,row.names=F)
      }
    )

  })
}

## To be copied in the UI
# mod_tab_extraction_ui("tab_extraction_1")

## To be copied in the server
# mod_tab_extraction_server("tab_extraction_1")
