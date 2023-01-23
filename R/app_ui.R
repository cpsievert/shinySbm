# mytheme <- fresh::create_theme(
#   fresh::adminlte_color(
#     light_blue = "#000000"
#   ),
#   fresh::adminlte_sidebar(
#     width = "200px",
#     dark_bg = "#9e999c",
#     dark_hover_bg = "#4d4b4b",
#     dark_color = "#000000",
#     dark_submenu_bg = "#7b7b7b",
#     dark_submenu_color = "#000000"
#   ),
#   fresh::adminlte_global(
#     content_bg = "#FFF",
#     box_bg = "#ffffff",
#     info_box_bg = "#ffffff"
#   )
# )


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
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(title = "ShinySBM"),
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("Data Loading", tabName = "tab_upload", icon = icon("th")),
          shinydashboard::menuItem("Data Plots", tabName = "tab_show", icon = icon("eye",lib="font-awesome")),
          shinydashboard::menuItem("SBM application", tabName = "tab_sbm", icon = icon("cogs",lib="font-awesome")),
          shinydashboard::menuItem("Network Plots", tabName = "tab_network", icon = icon("share-alt",lib="font-awesome")),
          shinydashboard::menuItem("Generated Groups", tabName = "tab_extraction", icon = icon("line-chart",lib="font-awesome"))
      )),
      ### DATA IMPORTATION
      # Probleme : dev les moyens de lecture du tableau comme dans blockmodelingGUI
      # introduction des covariables
      # no conditionalpanel work !!!
      shinydashboard::dashboardBody(
        # fresh::use_theme(mytheme),
        shinydashboard::tabItems(
          shinydashboard::tabItem(tabName = 'tab_upload',
                                  mod_tab_upload_ui("tab_upload_1")),

                    ### DATA SHOW
          shinydashboard::tabItem(tabName = 'tab_show',
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
          shinydashboard::tabItem(tabName = 'tab_sbm',

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
          shinydashboard::tabItem(tabName = 'tab_network',

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
                             plotOutput("networkPlot"))),
          ### GEN GROUPS
          shinydashboard::tabItem(tabName = 'tab_extraction')
          ))))
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
