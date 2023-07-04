#' prePlotNet
#'
#' @description A fct that build the settings for all plotNetMethods
#' @return list of settings
#'
#' @noRd
prePlotNet <- function(matrix,
                       is_bipartite,
                       labels,
                       directed,
                       settings){
  # default directed
  if(is.logical(directed)){
    currentDirected <- directed
  }else{
    if(directed != "default"){
      warning("directed should be a boolean or 'default'")
    }
    currentDirected <- isSymmetric(matrix)
  }

  # Default settings
  currentSettings <- list(
    node_list = NULL,
    edge_threshold = "default",
    edge_color = "lightblue",
    arrows = currentDirected,
    arrow_thickness = 0.3,
    arrow_start = 'row',
    node_color = if(is_bipartite){
      c(row = "orange",col = "blue")
    }else{
      c("blue")
    },
    node_shape = if(is_bipartite){
      c(row = "triangle",col = "square")
    }else{
      c("dot")
    },
    digits = 2
  )
  currentSettings[names(settings)] <- settings

  # Default labels
  if (identical(labels,"default")) {
    if(is_bipartite){
      currentLabels <- c(row = "row", col = "col")
    }else{
      currentLabels <- c(row = "nodes", col = "nodes")
    }
  }else{
    if(is_bipartite){
      currentLabels <- labels
    }else{
      currentLabels <- c(row = labels, col = labels)
    }
  }

  # Arrows default
  current_arrow <- list(enabled = TRUE,
                        scaleFactor = currentSettings$arrow_thickness)
  if(currentSettings$arrows){
    if(!currentSettings$arrow_start %in% c('row','col')){
      if(is_bipartite & currentSettings$arrow_start %in% currentLabels){
        currentSettings$arrow_start <- names(which(currentLabels == currentSettings$arrow_start))
      }else{
        warning(paste0("settings[['arrow_start']] should be 'row' or 'col'",
        ifelse(is_bipartite,"or any values of labels parameter","")))
      }
    }
    if(currentSettings$arrow_start  == 'row'){
      currentSettings$arrows <- list(from = current_arrow)
    }else if(currentSettings$arrow_start  == 'col'){
      currentSettings$arrows <- list(to = current_arrow)
    }else{
      currentSettings$arrows <- character()
    }
  }else{
    currentSettings$arrows <- character()
  }


  return(
    c(currentSettings,
      list(labels = currentLabels, directed = currentDirected ))
    )
}


#' visSbm
#'
#' @description A fct that plot a {visNetwork} plot of a adjacency matrix or an Sbm fit from the {sbm} package.
#'
#' @param x  : Sbm model of class `BipartiteSBM_fit`, `SimpleSBM_fit` or simple numeric `matrix`.
#' @param labels : labels for nodes. If it's simple sbm it should be a single character ("default" -> c("nodes")). If sbm is bipartite a named character (names are row and col) ("default" -> c(row = 'row', col = 'col')).
#' @param directed : Boolean indicating whether or not the network is directed by default, a asymmetrical matrix will be seen as directed.
#' @param settings : list of settings
#'
#' @details The list of parameters \code{plotOptions} for the matrix plot is
#' \itemize{
#'  \item{"arrow_start": }{character : "from" the arrow strat from first column and "to" the arrow start from tahe second column}
#'  \item{"edge_color": }{character : color of arrows}
#'  \item{"row_color": }{character : color for rows in case of bipartite}
#'  \item{"row_shape": }{character : shape of rows according to vsiNetwork shape agrument ("triangle", "square", etc...)}
#'  \item{"col_color": }{character : color for columns in case of bipartite}
#'  \item{"col_shape": }{character : shape of columns according to vsiNetwork shape agrument ("triangle", "square", etc...)}
#' }
#'
#'
#' @return a visNetwork plot
#'
#' @examples
#' data_bi <- sbm::fungusTreeNetwork$fungus_tree
#' my_sbm_bi <- sbm::estimateBipartiteSBM(data_bi)
#'
#'
#' visSbm(my_sbm_bi)
#'
#' data_uni <- sbm::fungusTreeNetwork$tree_tree
#' my_sbm_uni <- sbm::estimateSimpleSBM(data_uni,model = "poisson")
#' visSbm(my_sbm_uni)
#'
#'
#' @export
visSbm <- function(x,
                   labels = "default",
                   directed = "default",
                   settings = list()) {
  UseMethod("visSbm", x)
}

#' visSbm
#'
#' @description A fct that plot a {visNetwork} plot of a adjacency matrix or an Sbm fit from the {sbm} package.
#'
#' @param x  : Sbm model of class `BipartiteSBM_fit`, `SimpleSBM_fit` or simple numeric `matrix`.
#' @param labels : labels for nodes. If it's simple sbm it should be a single character ("default" -> c("nodes")). If sbm is bipartite a named character (names are row and col) ("default" -> c(row = 'row', col = 'col')).
#' @param directed : Boolean indicating whether or not the network is directed by default, a asymmetrical matrix will be seen as directed.
#' @param settings : list of settings
#'
#' @details The list of parameters \code{plotOptions} for the matrix plot is
#' \itemize{
#'  \item{"arrow_start": }{character : "from" the arrow strat from first column and "to" the arrow start from tahe second column}
#'  \item{"edge_color": }{character : color of arrows}
#'  \item{"row_color": }{character : color for rows in case of bipartite}
#'  \item{"row_shape": }{character : shape of rows according to vsiNetwork shape agrument ("triangle", "square", etc...)}
#'  \item{"col_color": }{character : color for columns in case of bipartite}
#'  \item{"col_shape": }{character : shape of columns according to vsiNetwork shape agrument ("triangle", "square", etc...)}
#' }
#'
#'
#' @return a visNetwork plot
#'
#'
#' @export
visSbm.default <- function(x,
                            labels = "default",
                            directed = "default",
                            settings = list()){
  stop('x should be  matrix of an sbm fit from {sbm} package')
}



#' visSbm
#'
#' @description A fct that plot a {visNetwork} plot of a adjacency matrix or an Sbm fit from the {sbm} package.
#'
#' @param x  : Sbm model of class `BipartiteSBM_fit`, `SimpleSBM_fit` or simple numeric `matrix`.
#' @param labels : labels for nodes. If it's simple sbm it should be a single character ("default" -> c("nodes")). If sbm is bipartite a named character (names are row and col) ("default" -> c(row = 'row', col = 'col')).
#' @param directed : Boolean indicating whether or not the network is directed by default, a asymmetrical matrix will be seen as directed.
#' @param settings : list of settings
#'
#' @details The list of parameters \code{plotOptions} for the matrix plot is
#' \itemize{
#'  \item{"arrow_start": }{character : "from" the arrow strat from first column and "to" the arrow start from tahe second column}
#'  \item{"edge_color": }{character : color of arrows}
#'  \item{"row_color": }{character : color for rows in case of bipartite}
#'  \item{"row_shape": }{character : shape of rows according to vsiNetwork shape agrument ("triangle", "square", etc...)}
#'  \item{"col_color": }{character : color for columns in case of bipartite}
#'  \item{"col_shape": }{character : shape of columns according to vsiNetwork shape agrument ("triangle", "square", etc...)}
#' }
#'
#'
#' @return a visNetwork plot
#'
#' @examples
#' data_bi <- sbm::fungusTreeNetwork$fungus_tree
#' my_sbm_bi <- sbm::estimateBipartiteSBM(data_bi)
#'
#'
#' visSbm(my_sbm_bi)
#'
#'
#'
#' @export
visSbm.BipartiteSBM_fit <- function(x,
                           labels = "default",
                           directed = "default",
                           settings = list()){



  preSettings <- prePlotNet(matrix = x$networkData,
                            is_bipartite = T,
                            labels = labels,
                            directed = F,
                            settings = settings)

  node_edges <- get_graph(x,preSettings$labels) %>%
    graph_filter(preSettings$edge_threshold)

  # Edges and Nodes Hoovering Sentence
  node_edges$edges$title <- paste("connectivity =",round(node_edges$edges$value,preSettings$digits))
  node_edges$nodes$title <- paste("proportion =",round_proportion(node_edges$nodes$value,preSettings$digits))

  visual <- visNetwork::visNetwork(node_edges$nodes, node_edges$edges) %>%
    visNetwork::visEdges(arrows = preSettings$arrows,
                         color = preSettings$edge_color,
                         arrowStrikethrough = F) %>%
    # darkblue square with shadow for group "A"
    visNetwork::visGroups(groupname = "row",
                          color = list(background = preSettings$node_color[["row"]],
                                       highlight = list(border = "black",
                                                     background = preSettings$node_color[["row"]])),
                          shape = preSettings$node_shape[["row"]]) %>%
    # red triangle for group "B"
    visNetwork::visGroups(groupname = "col",
                          color = list(background = preSettings$node_color[["col"]],
                                       highlight = list(border = "black",
                                                     background = preSettings$node_color[["col"]])),
                          shape = preSettings$node_shape[["col"]]) %>%
    visNetwork::visPhysics(solver = "forceAtlas2Based") %>%
    visNetwork::visHierarchicalLayout() %>%
    visNetwork::visInteraction(keyboard = TRUE)

  if(!is.null(preSettings$node_list)){
    visual <- visNetwork::visEvents(visual,selectNode = "function(properties) {
      alert('selected nodes ' + this.body.data.nodes.get(properties.nodes[0]).title);}")
  }
 return(visual)

}

#' visSbm
#'
#' @description A fct that plot a {visNetwork} plot of a adjacency matrix or an Sbm fit from the {sbm} package.
#'
#' @param x  : Sbm model of class `BipartiteSBM_fit`, `SimpleSBM_fit` or simple numeric `matrix`.
#' @param labels : labels for nodes. If it's simple sbm it should be a single character ("default" -> c("nodes")). If sbm is bipartite a named character (names are row and col) ("default" -> c(row = 'row', col = 'col')).
#' @param directed : Boolean indicating whether or not the network is directed by default, a asymmetrical matrix will be seen as directed.
#' @param settings : list of settings
#'
#' @details The list of parameters \code{plotOptions} for the matrix plot is
#' \itemize{
#'  \item{"arrow_start": }{character : "from" the arrow strat from first column and "to" the arrow start from tahe second column}
#'  \item{"edge_color": }{character : color of arrows}
#'  \item{"row_color": }{character : color for rows in case of bipartite}
#'  \item{"row_shape": }{character : shape of rows according to vsiNetwork shape agrument ("triangle", "square", etc...)}
#'  \item{"col_color": }{character : color for columns in case of bipartite}
#'  \item{"col_shape": }{character : shape of columns according to vsiNetwork shape agrument ("triangle", "square", etc...)}
#' }
#'
#'
#' @return a visNetwork plot
#'
#' @examples
#'
#' data_uni <- sbm::fungusTreeNetwork$tree_tree
#' my_sbm_uni <- sbm::estimateSimpleSBM(data_uni,model = "poisson")
#' visSbm(my_sbm_uni)
#'
#'
#' @export
visSbm.SimpleSBM_fit <- function(x,
                                  labels = "default",
                                  directed = "default",
                                  settings = list()){

  preSettings <- prePlotNet(matrix = x$networkData,
                            is_bipartite = F,
                            labels = labels,
                            directed = directed,
                            settings = settings)
  node_edges <- get_graph(x,preSettings$labels) %>%
    graph_filter(preSettings$edge_threshold)

  # Edges and Nodes Hoovering Sentence
  node_edges$edges$title <- paste("connectivity =",round(node_edges$edges$value,preSettings$digits))
  node_edges$nodes$title <- paste("proportion =",round_proportion(node_edges$nodes$value,preSettings$digits))


  visual <- visNetwork::visNetwork(node_edges$nodes, node_edges$edges) %>%
  visNetwork::visEdges(arrows = preSettings$arrows,
                       color = preSettings$edge_color,
                       arrowStrikethrough = F) %>%
  visNetwork::visNodes(title = ,
                       color = list(background = preSettings$node_color,
                                    highlight = list(border = "black",
                                                  background = preSettings$node_color)),
                       shape = preSettings$node_shape) %>%
    visNetwork::visPhysics(solver = "forceAtlas2Based") %>%
    visNetwork::visInteraction(keyboard = TRUE)

  if(!is.null(preSettings$node_list)){
    visual <- visNetwork::visEvents(visual,selectNode = "function(properties) {
      alert('selected nodes ' + this.body.data.nodes.get(properties.nodes[0]).title);}")
  }
  return(visual)
}

