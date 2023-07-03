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
build_node_edge.SimpleSBM_fit <- function(sbmObject, labels, directed = F, ...) {
  nb_nodes <- sbmObject$nbBlocks
  id <- 1:nb_nodes
  # Build nodes tables
  nodes <- data.frame(
    id = id, # one id for each group
    label = paste0(labels[["row"]], " group ", id), # Name of the group
    value = sbmObject$blockProp # Group size
  )
  connection_matrix <- sbmObject$connectParam$mean
  # I the matrix isn't symmetric or we want to force it to be treated as an directed matrix
  if (isSymmetric(connection_matrix) & !directed) {
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
    `names<-`(c(labels[["row"]], labels[["col"]]))
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
    from = paste0(labels[["col"]], rep(id[[labels[["col"]]]], each = nb_nodes[[labels[["row"]]]])),
    to = paste0(labels[["row"]], rep(id[[labels[["row"]]]], nb_nodes[[labels[["col"]]]])),
    value = as.vector(connection_matrix)
  )

  return(list(nodes = nodes, edges = edges, type = "bipartite"))
}

#' build_node_edge.matrix method
#'
#' @description A fct that build a structure for network visualisation
#' @return list of dataframe for nodes and edges of the graphs
#'
#' @noRd
build_node_edge.matrix <- function(sbmObject,
                                   nodes_names = list(
                                     row = rownames(sbmObject),
                                     col = colnames(sbmObject)
                                   ),
                                   type = "unipartite", directed = F, ...) {
  ## Tests
  if (dim(sbmObject)[[1]] != length(nodes_names[["row"]]) | dim(sbmObject)[[2]] != length(nodes_names[["col"]])) {
    stop("sbmObject has different dimension than nodes_names")
  }
  if (var(dim(sbmObject)) != 0 & type == "unipartite") {
    stop("sbmObject has different number of raws and columns, it can't be unipartite")
  }
  if (var(dim(sbmObject)) == 0 & type == "bipartite") {
    message("sbmObject has same number of raws and columns are you sure this network is bipartite ?")
  }
  if ((length(nodes_names[["row"]]) != length(nodes_names[["col"]]) || any(nodes_names[["row"]] != nodes_names[["col"]])) & type == "unipartite") {
    warnings("nodes_names has two differents types are you sur the network is unipartite")
  }
  if (isSymmetric(sbmObject) & directed) {
    warnings("sbmObject is symmetric are you sure it is directed")
  }
  if (!isSymmetric(sbmObject) & !directed) {
    warnings("sbmObject isn't symmetric are you sure the network isn't directed")
  }

  if (type == "unipartite") {
    labs <- data.frame(label = nodes_names[["col"]])
  } else {
    labs <- dplyr::bind_rows(purrr::map(
      c("row", "col"),
      ~ setNames(
        data.frame(nodes_names[.x], .x),
        c("label", "lab_type")
      )
    ))
  }

  nodes <- data.frame(
    id = 1:length(labs$label),
    label = labs$label
  )
  if (type == "bipartite") {
    nodes <- nodes %>%
      dplyr::mutate(
        level = ifelse(labs$lab_type == "row", 1, 2),
        shape = ifelse(labs$lab_type == "row", "triangle", "square")
      )

    edges <- data.frame(
      from = rep(nodes$id[nodes$level == 1], sum(nodes$level == 2)),
      to = rep(nodes$id[nodes$level == 2], each = sum(nodes$level == 1)),
      value = as.vector(sbmObject)
    )
  } else {
    # I the matrix isn't symmetric or we want to force it to be treated as an oriented matrix
    if (isSymmetric(sbmObject) & !directed) {
      # Edges table
      edges <- data.frame(
        # edges start from
        from = sapply(nodes$id, function(i) { # for each group nb
          rep(i, each = length(labs$label) - i + 1) # connection with the ones it is not yet connected
        }) %>% unlist(),
        # edges end to
        to = sapply(nodes$id, function(i) { # for each group nb
          i:length(labs$label) # connection with the ones it is not yet connected
        }) %>% unlist()
      ) %>%
        dplyr::mutate(value = apply(., 1, function(i) {
          sbmObject[i[1], i[2]] # get connection values
        }) %>% unlist())
    } else {
      # if matrix isn't symertic or treated as asymetric
      edges <- data.frame(
        from = rep(nodes$id, length(labs$label)),
        to = rep(nodes$id, each = length(labs$label)),
        value = as.vector(sbmObject)
      )
    }
  }
  return(list(nodes = nodes, edges = edges, type = type))
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
