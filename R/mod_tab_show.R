#' tab_show UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_show_ui <- function(id) {
  ns <- NS(id)
  ns_tab_sbm <- function(id) {
    paste0("tab_sbm_1-", id)
  }
  tagList(
    shinydashboard::box(
      title = "Visualisation settings", solidHeader = T,
      status = "info", collapsible = T,
      radioButtons(ns("whichShow"), "Type of visualisation",
        choices = list(
          "Print a table" = "print",
          "Plot a matrix" = "plot"
        ),
        inline = T
      ),
      radioButtons(ns("whichRawSbmMatrix"), "Select Ploted Matrix",
        choices = list("Raw Matrix" = "raw")
      )
    ),
    conditionalPanel(
      condition = "input.runSbm", ns = ns_tab_sbm,
      shinydashboard::box(
        title = "Block settings", solidHeader = T,
        status = "info", collapsible = T,
        mod_select_nb_groups_ui(ns("select_nb_groups_1"))
      )
    ),
    shinydashboard::box(
      title = "Plots", solidHeader = T,
      status = "info", width = 12,
      conditionalPanel(
        condition = "input.whichShow == 'print'", ns = ns,
        DT::dataTableOutput(ns("matrixPrint")),
        tags$head(tags$style(css_big_table(ns("matrixPrint"))))
      ),
      conditionalPanel(
        condition = "input.whichShow != 'print'", ns = ns,
        imageOutput(ns("matrixPlot"))
      )
    )
  )
}

#' tab_show Server Functions
#'
#' @noRd
mod_tab_show_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    mod_select_nb_groups_res <- mod_select_nb_groups_server(
      "select_nb_groups_1",
      r$sbm$main_sbm, r$sbm$NbBlocks
    )
    my_sbm <- mod_select_nb_groups_res$my_sbm

    output$matrixPrint <- DT::renderDataTable({
      # probleme : taille et position, wrapping des titres, fixer la colonnne de rownames
      req(input$whichShow)
      if (input$whichShow != "print") {
        return(NULL)
      }
      DT::datatable(
        as.data.frame(r$upload$Dataset()),
        extensions = c("FixedColumns", "FixedHeader", "KeyTable"),
        option = list(
          fixedHeader = TRUE,
          fixedColumns = list(leftColumns = 1),
          scrollX = T,
          scrollY = T,
          keys = TRUE,
          paging = F
        )
      ) %>%
        DT::formatStyle(c(1:dim(r$upload$Dataset())[2]), border = "1px solid #ddd")
    })

    PlotMat <- reactive({
      req(input$whichShow)
      if (input$whichShow == "plot") {
        x <- as.matrix(r$upload$Dataset())
        labels_list <- list()
        if (dim(x)[1] > dim(x)[2]) {
          labels_list$row <- r$upload$labels()$col
          labels_list$col <- r$upload$labels()$row
          x <- t(x)
        } else {
          labels_list <- r$upload$labels()
        }
        if (!is.null(r$sbm$runSbm()) && r$sbm$runSbm() != 0) {
          data_sbm <- my_sbm()$clone()
          switch(input$whichRawSbmMatrix,
            "raw" = sbm::plotMyMatrix(x, dimLabels = labels_list),
            "ordered" = plot(data_sbm, type = "data", dimLabels = r$upload$labels()),
            "simple" = plot(data_sbm, type = "expected", dimLabels = r$upload$labels())
          )
        } else {
          sbm::plotMyMatrix(Mat = x, dimLabels = labels_list)
        }
      } else {
        return(NULL)
      }
    })


    output$matrixPlot <- renderImage(
      {
        # Read myImage's width and height. These are reactive values, so this
        # expression will re-run whenever they change.
        width <- session$clientData$`output_tab_show_1-matrixPlot_width`
        height <- session$clientData$`output_tab_show_1-matrixPlot_height`

        # For high-res displays, this will be greater than 1
        pixelratio <- session$clientData$pixelratio

        # A temp file to save the output.
        outfile <- tempfile(fileext = ".png")

        # Generate the image file
        png(outfile,
          width = width * pixelratio, height = height * pixelratio,
          res = 72 * pixelratio
        )
        plot(PlotMat())
        dev.off()

        # Return a list containing the filename
        list(
          src = outfile,
          width = width,
          height = height
        )
      },
      deleteFile = TRUE
    )

    return(list(
      NbBlocks = mod_select_nb_groups_res$Nbblocks
    ))
  })
}








## To be copied in the UI
# mod_tab_show_ui("tab_show_1")

## To be copied in the server
# mod_tab_show_server("tab_show_1")
