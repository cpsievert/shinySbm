#' edges_to_adjacency
#'
#' @description A fct that build an adjacency matrix from a list of pair of nodes
#'
#' @param my_list,type=c('unipartite',"bipartite"),oriented=T
#' `my_list` a data.frame which is a list pair of nodes (nodes ids are one the two first columns) a numerical third column can be associated will be the connections values.
#' `type` network type can be `'bipartite'` or `'unipartite'`
#' `oriented` boolean : whether or not connections are oriented (`T`) or symetrical (`F`)
#'
#'
#' @return an adjacency/incidence matrix (data.frame) representing the network
#'
#' @noRd
edges_to_adjacency <- function(my_list, type = c("unipartite", "bipartite"), oriented = T) {
  ## Rename columns of the pair of node list by 'from', 'to' and 'value' (if needed)
  if (dim(my_list)[2] == 2) {
    names(my_list) <- c("from", "to")
  } else if (dim(my_list)[2] == 3) {
    names(my_list) <- c("from", "to", "value")
  } else {
    message("my_list should be a data.frame of 2 or 3 columns")
    return()
  }

  ## According to the type of network while define differently the nodes names
  if (type[1] == "unipartite") {
    # Unipartite case the nodes are the same for cols and rows
    name_row <- name_col <- sort(unique(c(my_list$from, my_list$to)))
  } else if (type[1] == "bipartite") {
    # bipartite differnt names for from/rows and to:cols
    name_row <- sort(unique(my_list$from))
    name_col <- sort(unique(my_list$to))
  } else {
    stop("type should be 'bipartite' or 'unipartite'")
  }

  # Empty named matrix
  mat <- matrix(0, length(name_row), length(name_col))
  rownames(mat) <- name_row
  colnames(mat) <- name_col
  # Changing from and to colums with positions into the matrix
  my_list <- as.matrix(dplyr::mutate(my_list,
    from = match(from, name_row),
    to = match(to, name_col)
  ))
  # Set values in right positions
  mat[my_list[, 1:2]] <- ifelse(rep(dim(my_list)[2] == 2, dim(my_list)[1]),
    1, my_list[, 3]
  )
  # Specific case of unipartite network with symmetrical connections,
  # just doing the same but reversing to and from columns
  if (type[1] == "unipartite" & !oriented) {
    mat[my_list[, 2:1]] <- ifelse(rep(dim(my_list)[2] == 2, dim(my_list)[1]),
      1, my_list[, 3]
    )
  }
  return(as.data.frame(mat))
}
