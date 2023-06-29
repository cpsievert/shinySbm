#' round_prop
#'
#' @description A fct that round a proportion vector and keep sum at one
#'
#' @return the rounded vector
#'
#' @noRd
round_prop <- function(x, digits = 2) {
  if(length(x) == 1){
    return(1)
  }
  prop_vec <- round(x, digits)
  prop_vec[length(prop_vec)] <- 1 - sum(prop_vec[1:(length(prop_vec) - 1)])
  return(prop_vec)
}

#' flexBlockProp
#'
#' @description A fct that gives a nice flextable for block proportion from the sbm param
#'
#' @return The block proportions in flextable
#'
#' @noRd
flexBlockProp <- function(sbm, labels,
                          caption = "Table 1: Block proportions") {
  ## Data
  if (is.bipartite(sbm)) {
    row <- round_prop(sbm$blockProp$row)
    col <- round_prop(sbm$blockProp$col)
    n <- max(length(row), length(col))
    length(row) <- n
    length(col) <- n
    data_prop <- data.frame(
      Blocks = 1:n,
      row = row,
      col = col
    ) %>%
      setNames(c("Blocks", labels[["row"]], labels[["col"]]))
  } else {
    nodes <- round_prop(sbm$blockProp)
    data_prop <- data.frame(
      Blocks = 1:length(nodes),
      nodes = nodes
    ) %>%
      setNames(c("Blocks", labels[["row"]]))
  }
  ## Build flextable
  ft <- flextable::flextable(data_prop) %>%
    flextable::set_caption(caption = caption) %>%
    flextable::theme_vanilla() %>%
    flextable::autofit()
  return(ft)
}


#' flexConnect
#'
#' @description A fct that gives a nice flextable for connectivity from the sbm param
#'
#' @return The connectivity in flextable
#'
#' @noRd
flexConnect <- function(sbm, labels,
                        caption = "Table 2: Connectivity betweens blocks") {
  ## Data
  data_connect <- as.data.frame(sbm$connectParam$mean) %>%
    dplyr::mutate_all(~round(.x,2)) %>%
    setNames(1:length(.)) %>%
    dplyr::mutate(rowlabel = labels[["row"]], rowNb = 1:nrow(.), .before = 1)

  ## Build flex
  ft <- flextable::flextable(data_connect) %>%
    flextable::merge_v(1)
  if(nrow(data_connect) != 1){
    ft <- flextable::rotate(ft,j = 1, rotation = "btlr", align = "center")
  }

  ## Add variance parmeter
  if (sbm$modelName == "gaussian") {
    if(length(data_connect) == 3){
      colwidths <-  rep(1,3)
    }else{
      colwidths <- c(2, 1, length(data_connect) - 3)
    }
    ft <- flextable::add_footer_row(ft,
                                    values = c("Variance", round(sbm$connectParam$var[[1]], 3), ""),
                                    colwidths = colwidths
    ) %>%
      flextable::bold(j = 1, part = "footer")
  }

  ## Polish table
  ft <- flextable::theme_vanilla(ft) %>%
    flextable::bold(j = 1:2) %>%
    flextable::void(j = 1:2, part = "header") %>%
    flextable::add_header_row(
      values = c("", "", labels[["col"]]),
      colwidths = c(1, 1, length(data_connect) - 2)
    ) %>%
    flextable::align(i = 1, j = NULL, align = "center", part = "header") %>%
    flextable::align(j = 1, align = "right", part = "footer") %>%
    flextable::border(i = 1,j=1:2,border = flextable::fp_border_default(width = 0),part = 'header') %>%
    flextable::border(i = 2,j=1:2,border.top = flextable::fp_border_default(width = 0),part = 'header') %>%
    flextable::set_caption(caption = caption) %>%
    flextable::autofit()

  return(ft)
}

#' flexConnect
#'
#' @description A fct that gives a nice flextable for stored Models from the sbm param
#'
#' @return lextable for stored Models
#'
#' @noRd
flexStoredModels <- function(sbm, labels,
                             caption = "Table 3: All explored models",
                             colorParam = list()) {
  ## Default colors
  currentParam <- list(
    selected_col = "orange",
    best_col = "red"
  )
  currentParam[names(colorParam)] <- colorParam

  ## Data
  data_strored <- as.data.frame(round(sbm$storedModels, 2)) %>%
    dplyr::select(-indexModel) %>%
    dplyr::relocate(nbParams, .after = everything())

  ## Additional info
  is_bip <- is.bipartite(sbm)
  selected_line <- which(data_strored$nbBlocks == sum(sbm$nbBlocks))
  best_line <- which.max(data_strored$ICL)

  ## Renames cols
  if (is_bip) {
    names(data_strored) <- c(
      paste0("Nb of ", labels[["row"]], " blocks"),
      paste0("Nb of ", labels[["col"]], " blocks"),
      "Total nb of blocks", "ICL", "log-Likelihood",
      "Nb of parameters"
    )
  } else {
    names(data_strored) <- c(
      "Total nb of blocks", "ICL", "log-Likelihood",
      "Nb of parameters"
    )
  }

  ## Build Flextable
  ft <- flextable::flextable(data_strored) %>%
    flextable::theme_vanilla() %>%
    flextable::color(i = best_line, color = currentParam$best_col) %>%
    flextable::bg(
      i = selected_line,
      bg = currentParam$selected_col
    ) %>%
    flextable::align(align = "center", part = "header") %>%
    flextable::set_caption(caption = caption) %>%
    flextable::autofit()

  return(ft)
}

