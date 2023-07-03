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
  # Default settings
  currentSettings <- list(
    arrow_start = "from",
    arrow_color = "lightblue",
    node_color = if(is_bipartite){
      c(row = "orange",col = "blue")
    }else{
      c("blue")
    },
    node_shape = if(is_bipartite){
      c(row = "triangle",col = "square")
    }else{
      c("circle")
    }
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
    currentLabels <- labels
  }

  # default directed
  if(is.logical(directed)){
    currentDirected <- directed
  }else{
    currentDirected <- isSymmetric(matrix)
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
#'  \item{"arrow_color": }{character : color of arrows}
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
#'  \item{"arrow_color": }{character : color of arrows}
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
#'  \item{"arrow_color": }{character : color of arrows}
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
  node_edges <- build_node_edge(x,preSettings$labels)


  visNetwork::visNetwork(node_edges$nodes, node_edges$edges) %>%
    visNetwork::visEdges(preSettings$arrow_start,
                         color = preSettings$arrow_color) %>%
    # darkblue square with shadow for group "A"
    visNetwork::visGroups(groupname = "row",
                          color = preSettings$node_color[["row"]],
                          shape = preSettings$node_shape[["row"]]) %>%
    # red triangle for group "B"
    visNetwork::visGroups(groupname = "col",
                          color = preSettings$node_color[["col"]],
                          shape = preSettings$node_shape[["col"]]) %>%
    visNetwork::visHierarchicalLayout()
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
#'  \item{"arrow_color": }{character : color of arrows}
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
  node_edges <- build_node_edge(x,preSettings$labels)

  visNetwork::visNetwork(node_edges$nodes, node_edges$edges)
}

