#' ILC_plot
#'
#' @description A fct that plot a the ILC curve and the position of two version
#'  of the same SBMmodel with or not differents K values (nb of groups)
#' @return no value
#'
#' @noRd
ILC_plot <- function(selected_sbm, comparison_sbm = selected_sbm) {
  xmin <- min(sum(selected_sbm$nbBlocks),sum(comparison_sbm$nbBlocks)) - 1
  xmax <- max(sum(selected_sbm$nbBlocks),sum(comparison_sbm$nbBlocks)) + 1
  plot_table <- dplyr::filter(selected_sbm$storedModels, nbBlocks <= xmax & nbBlocks >= xmin)
  my_plot <- ggplot2::ggplot(plot_table) +
    ggplot2::xlim(xmin,xmax) +
    ggplot2::aes(x = nbBlocks, y = ICL, linetype = "ICL") +
    ggplot2::geom_line() +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_line(ggplot2::aes(x = nbBlocks, y = loglik, linetype = "Log Likelihood")) +
    ggplot2::geom_point(ggplot2::aes(x = sum(selected_sbm$nbBlocks), y = selected_sbm$ICL, colour = "Selected Block Nb"), size = 4) +
    ggplot2::geom_point(ggplot2::aes(x = sum(comparison_sbm$nbBlocks), y = comparison_sbm$ICL, colour = "Best Block Nb"), size = 4, shape = 10) +
    ggplot2::labs(linetype = "Curves", colour = "Number of Blocks") +
    ggplot2::theme(legend.position="bottom",
                   legend.box = "vertical")
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
build_node_edge.SimpleSBM_fit <- function(sbmObject, labels, oriented = F, ...) {
  nb_nodes <- sbmObject$nbBlocks
  id <- 1:nb_nodes
  # Build nodes tables
  nodes <- data.frame(
    id = id, # one id for each group
    label = paste0(labels$row, " group ", id), # Name of the group
    value = sbmObject$blockProp # Group size
  )
  connection_matrix <- sbmObject$connectParam$mean
  # I the matrix isn't symmetric or we want to force it to be treated as an oriented matrix
  if (isSymmetric(connection_matrix) & !oriented) {
    # Edges table
    edges <- data.frame(
      # edges start from
      from = sapply(id, function(i) { # for each group nb
        rep(i, each = nb_nodes - i + 1) # connection with the ones it is not yet connected
      }) %>% unlist(),
      # edges end to
      to = sapply(id, function(i) { # for each group nb
        i:nb_nodes # connection with the ones it is not yet connected
      }) %>% unlist()
    ) %>%
      dplyr::mutate(value = apply(., 1, function(i) {
        connection_matrix[i[1], i[2]] # get connection values
      }) %>% unlist())
  } else {
    # if matrix isn't symertic or treated as asymetric
    edges <- data.frame(
      from = rep(id, each = nb_nodes),
      to = rep(id, nb_nodes),
      value = as.vector(connection_matrix)
    )
  }
  return(list(nodes = nodes, edges = edges, type = "unipartite"))
}

### I need to correct the nodes names and groups


#' build_node_edge.BipartiteSBM_fit method
#'
#' @description A fct that build a structure for network visualisation
#' @return list of dataframe for nodes and edges of the graphs
#'
#' @noRd
build_node_edge.BipartiteSBM_fit <- function(sbmObject, labels, ...) {

  nb_nodes <- sbmObject$nbBlocks %>%
    as.list() %>%
    `names<-`(c(labels$row, labels$col))
  id <- lapply(nb_nodes, function(i) {
    1:i
  })

  nodes <- data.frame(
    id = id %>% unlist() %>% names(),
    value = sbmObject$blockProp %>% unlist(),
    group = lapply(1:2, function(i) {
      rep(names(labels)[[i]], each = nb_nodes[[i]])
    }) %>% unlist()
  ) %>%
    dplyr::mutate(
      label = paste("Group", id),
      level = ifelse(group == "row", 1, 2),
      shape = ifelse(group == "row", "triangle", "square")
    ) %>%
    dplyr::select(id, label, level, value, group)


  connection_matrix <- sbmObject$connectParam$mean

  edges <- data.frame(
    from = paste0(labels$col, rep(id[[labels$col]], each = nb_nodes[[labels$row]])),
    to = paste0(labels$row, rep(id[[labels$row]], nb_nodes[[labels$col]])),
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
