#' get_graph generic
#'
#' @description A fct that build a structure for network visualisation
#' @return list of dataframe for nodes and edges of the graphs
#'
#' @noRd
get_graph <- function(x, ...) {
  UseMethod("get_graph", object = x)
}

#' get_graph.SimpleSBM_fit method
#'
#' @description A fct that build a structure for network visualisation
#' @return list of dataframe for nodes and edges of the graphs
#'
#' @noRd
get_graph.SimpleSBM_fit <- function(x, labels, directed = F, ...) {
  nb_nodes <- x$nbBlocks
  id <- 1:nb_nodes
  # Build nodes tables
  nodes <- data.frame(
    id = id, # one id for each block
    label = paste0(labels[["row"]], " block ", id), # Name of the block
    value = x$blockProp # block size
  )
  connection_matrix <- x$connectParam$mean
  # I the matrix isn't symmetric or we want to force it to be treated as an directed matrix
  if (isSymmetric(connection_matrix) & !directed) {
    # Edges table
    edges <- data.frame(
      # edges start from
      from = sapply(id, function(i) { # for each block nb
        rep(i, each = nb_nodes - i + 1) # connection with the ones it is not yet connected
      }) %>% unlist(),
      # edges end to
      to = sapply(id, function(i) { # for each block nb
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

### I need to correct the nodes names and blocks


#' get_graph.BipartiteSBM_fit method
#'
#' @description A fct that build a structure for network visualisation
#' @return list of dataframe for nodes and edges of the graphs
#'
#' @noRd
get_graph.BipartiteSBM_fit <- function(x, labels, ...) {
  nb_nodes <- x$nbBlocks %>%
    as.list() %>%
    setNames(c(labels[["row"]], labels[["col"]]))
  id <- lapply(nb_nodes, function(i) {
    1:i
  })

  nodes <- data.frame(
    id = id %>%
      purrr::map2(.x = names(.), .y = .,.f = ~paste(.x,.y)) %>%
      unlist(),
    value = x$blockProp %>%
      unlist(),
    group = lapply(1:2, function(i) {
      rep(names(labels)[[i]], each = nb_nodes[[i]])
    }) %>% unlist()
  ) %>%
    dplyr::mutate(
      label = paste("Block", id),
      level = ifelse(group == "row", 1, 2),
      shape = ifelse(group == "row", "triangle", "square")
    ) %>%
    dplyr::select(id, label, level, value, group)


  connection_matrix <- x$connectParam$mean

  edges <- data.frame(
    from = paste0(labels[["col"]],' ', rep(id[[labels[["col"]]]], each = nb_nodes[[labels[["row"]]]])),
    to = paste0(labels[["row"]],' ', rep(id[[labels[["row"]]]], nb_nodes[[labels[["col"]]]])),
    value = as.vector(connection_matrix)
  )

  return(list(nodes = nodes, edges = edges, type = "bipartite"))
}

#' get_graph.matrix method
#'
#' @description A fct that build a structure for network visualisation
#' @return list of dataframe for nodes and edges of the graphs
#'
#' @noRd
get_graph.matrix <- function(x,
                                   nodes_names = list(
                                     row = rownames(x),
                                     col = colnames(x)
                                   ),
                                   type = "unipartite", directed = F, ...) {
  ## Tests
  if (dim(x)[[1]] != length(nodes_names[["row"]]) | dim(x)[[2]] != length(nodes_names[["col"]])) {
    stop("x has different dimension than nodes_names")
  }
  if (var(dim(x)) != 0 & type == "unipartite") {
    stop("x has different number of raws and columns, it can't be unipartite")
  }
  if (var(dim(x)) == 0 & type == "bipartite") {
    message("x has same number of raws and columns are you sure this network is bipartite ?")
  }
  if ((length(nodes_names[["row"]]) != length(nodes_names[["col"]]) || any(nodes_names[["row"]] != nodes_names[["col"]])) & type == "unipartite") {
    warnings("nodes_names has two differents types are you sur the network is unipartite")
  }
  if (isSymmetric(x) & directed) {
    warnings("x is symmetric are you sure it is directed")
  }
  if (!isSymmetric(x) & !directed) {
    warnings("x isn't symmetric are you sure the network isn't directed")
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
      value = as.vector(x)
    )
  } else {
    # I the matrix isn't symmetric or we want to force it to be treated as an oriented matrix
    if (isSymmetric(x) & !directed) {
      # Edges table
      edges <- data.frame(
        # edges start from
        from = sapply(nodes$id, function(i) { # for each block nb
          rep(i, each = length(labs$label) - i + 1) # connection with the ones it is not yet connected
        }) %>% unlist(),
        # edges end to
        to = sapply(nodes$id, function(i) { # for each block nb
          i:length(labs$label) # connection with the ones it is not yet connected
        }) %>% unlist()
      ) %>%
        dplyr::mutate(value = apply(., 1, function(i) {
          x[i[1], i[2]] # get connection values
        }) %>% unlist())
    } else {
      # if matrix isn't symertic or treated as asymetric
      edges <- data.frame(
        from = rep(nodes$id, length(labs$label)),
        to = rep(nodes$id, each = length(labs$label)),
        value = as.vector(x)
      )
    }
  }
  return(list(nodes = nodes, edges = edges, type = type))
}


#' graph_filter
#'
#' @description graph_filter
#' @return list of dataframe for nodes and edges of the graphs
#'
#' @noRd
graph_filter <- function(graph,threshold = 'default',filter_type = 'relative'){

  if(filter_type == 'relative'){
    if(threshold == 'default'){
      if(graph$type == 'bipartite'){
        value_threshold <- purrr::map_dbl(c('from','to'),function(col){
          graph$edges %>%
            dplyr::group_by_at(col) %>%
            dplyr::reframe(max = max(value)) %>%
            dplyr::pull(max) %>%
            min()
        }) %>% min()
      }else{
        value_threshold <- purrr::map_dfr(c('from','to'),function(col){
          test$edges %>%
            dplyr::select_at(c(col,'value')) %>%
            dplyr::rename_at(col,~'block')}) %>%
          dplyr::group_by(block) %>%
          dplyr::reframe(max = max(value)) %>%
          dplyr::pull(max) %>%
          min()
      }
    }else{
      value_threshold <- (max(graph$edges$value) - min(graph$edges$value)) *
        threshold + min(graph$edges$value)
      if(!(is.numeric(threshold) && 0 <= threshold && 1 >= threshold)){
        warning("threshold should be set as 'default' or a numeric value between 0 and 1")
      }
    }
  }else{
    value_threshold <- threshold
  }


  graph$edges <- graph$edges %>%
    dplyr::filter(value >= value_threshold)

  return(graph)
}

