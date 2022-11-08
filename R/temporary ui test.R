ui <- navbarPage(
  title = "Shiny for Stochastic block model",
  id = "main_tab",
  selected = "Line Plots",
  collapsible = TRUE,
  theme = bslib::bs_theme(),
  tabPanel(
    title = "Data Importation",
    grid_container(
      layout = "maindata separator",
      row_sizes = "1fr",
      col_sizes = c(
        "285px",
        "285px",
        "1fr"
      ),
      gap_size = "10px",
      grid_card_plot(area = "linePlots"),
      grid_card(
        area = "area1",
        radioButtons(
          inputId = "whichdata",
          label = "What kind of data do you want to use ?",
          choices = list(
            `Import my data` = "upload",
            `Sbm Exemple` = "exemple"
          ),
          width = "100%"
        ),
        textInput(
          inputId = "myTextInput",
          label = "Text Input",
          value = ""
        )
      )
    )
  ),
  tabPanel(
    title = "SBM application",
    grid_container(
      layout = c(
        "facetOption",
        "dists      "
      ),
      row_sizes = c(
        "165px",
        "1fr"
      ),
      col_sizes = "1fr",
      gap_size = "10px",
      grid_card_plot(area = "dists"),
      grid_card(
        area = "facetOption",
        title = "Distribution Plot Options",
        radioButtons(
          inputId = "distFacet",
          label = "Facet distribution by",
          choices = list(
            `Diet Option` = "Diet",
            `Measure Time` = "Time"
          ),
          width = "100%"
        )
      )
    )
  ),
  tabPanel(title = "Visualisation"),
  tabPanel(title = "Network exploration")
)
