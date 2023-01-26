#' ILC_plot
#'
#' @description A fct that plot a the ILC curve and the position of two version
#'  of the same SBMmodel with or not differents K values (nb of groups)
#' @return no value
#'
#' @noRd
ILC_plot <- function(selected_sbm, comparison_sbm = selected_sbm) {
  my_plot <- ggplot2::ggplot(selected_sbm$storedModels) +
    ggplot2::aes(x = nbBlocks, y = ICL, linetype = "ICL") +
    ggplot2::geom_line() +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_line(ggplot2::aes(x = nbBlocks, y = loglik, linetype = "Log Likelihood")) +
    ggplot2::geom_point(ggplot2::aes(x = sum(selected_sbm$nbBlocks), y = selected_sbm$ICL, colour = "Selected Block Nb"), size = 4) +
    ggplot2::geom_point(ggplot2::aes(x = sum(comparison_sbm$nbBlocks), y = comparison_sbm$ICL, colour = "Best Block Nb"), size = 4, shape = 10) +
    ggplot2::labs(linetype = "Curves", colour = "Number of Blocks") +
    ggplot2::theme(
      legend.position = c(.40, .05),
      legend.justification = c("left", "bottom"),
      legend.box.just = "left",
      legend.margin = ggplot2::margin(6, 6, 6, 6)
    )
  plot(my_plot)
}


#' build_node_edge generic
#'
#' @description A fct that build a structure for network visualisation
#' @return list of dataframe for nodes and edges of the graphs
#'
#' @noRd
build_node_edge <- function(sbmObject, ...) {
  UseMethod("build_node_edge", object = sbmObject)
}

#' build_node_edge.SimpleSBM_fit method
#'
#' @description A fct that build a structure for network visualisation
#' @return list of dataframe for nodes and edges of the graphs
#'
#' @noRd
build_node_edge.SimpleSBM_fit <- function(sbmObject, oriented = F, ...) {
  nb_nodes <- sbmObject$nbBlocks
  id <- 1:nb_nodes
  nodes <- data.frame(
    id = id,
    label = paste("Node", id),
    value = sbmObject$blockProp
  )
  connection_matrix <- sbmObject$connectParam$mean
  if (isSymmetric(connection_matrix) & !oriented) {
    edges <- data.frame(
      from = sapply(id, function(i) {
        rep(i, each = nb_nodes - i + 1)
      }) %>% unlist(),
      to = sapply(id, function(i) {
        i:nb_nodes
      }) %>% unlist()
    ) %>%
      dplyr::mutate(value = apply(., 1, function(i) {
        connection_matrix[i[1], i[2]]
      }) %>% unlist())
  } else {
    edges <- data.frame(
      from = rep(id, each = nb_nodes),
      to = rep(id, nb_nodes),
      value = as.vector(connection_matrix)
    )
  }
  return(list(nodes = nodes, edges = edges, type = "unipartite"))
}



#' build_node_edge.BipartiteSBM_fit method
#'
#' @description A fct that build a structure for network visualisation
#' @return list of dataframe for nodes and edges of the graphs
#'
#' @noRd
build_node_edge.BipartiteSBM_fit <- function(sbmObject, ...) {
  nb_nodes <- sbmObject$nbBlocks %>% as.list()
  id <- lapply(nb_nodes, function(i) {
    1:i
  })

  nodes <- data.frame(
    id = id %>% unlist() %>% names(),
    value = sbmObject$blockProp %>% unlist(),
    group = lapply(1:2, function(i) {
      rep(names(nb_nodes)[[i]], each = nb_nodes[[i]])
    }) %>% unlist()
  ) %>%
    dplyr::mutate(
      label = paste("Node", id),
      level = ifelse(group == "row", 1, 2),
      shape = ifelse(group == "row", "triangle", "square")
    ) %>%
    dplyr::select(id, label, level, value, group)


  connection_matrix <- sbmObject$connectParam$mean

  edges <- data.frame(
    from = paste0("col", rep(id$col, each = nb_nodes$row)),
    to = paste0("row", rep(id$row, nb_nodes$col)),
    value = as.vector(connection_matrix)
  )

  return(list(nodes = nodes, edges = edges, type = "bipartite"))
}


#' netPlot
#'
#' @description A fct that plot a network structure
#' @return list of dataframe for nodes and edges of the graphs
#'
#' @noRd
netPlot <- function(nodes, edges, type = "unipartite",
                    arrows = "from", arrows_color = "lightblue",
                    row_color = "orange", row_shape = "triangle",
                    col_color = "blue", col_shape = "square") {
  if (type == "unipartite") {
    visNetwork::visNetwork(nodes, edges)
  } else {
    visNetwork::visNetwork(nodes, edges) %>%
      visNetwork::visEdges(arrows, color = arrows_color) %>%
      # darkblue square with shadow for group "A"
      visNetwork::visGroups(groupname = "row", color = row_color, shape = row_shape) %>%
      # red triangle for group "B"
      visNetwork::visGroups(groupname = "col", color = col_color, shape = col_shape) %>%
      visNetwork::visHierarchicalLayout()
  }
}
