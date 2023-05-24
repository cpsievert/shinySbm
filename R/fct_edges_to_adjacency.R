#' edges_to_adjacency
#'
#' @description A fct that build an adjacency matrix from a list of pair of nodes
#'
#' @param my_list a data.frame which is a list pair of nodes (nodes ids are one the two first columns) a numerical third column can be associated will be the connections values.
#' @param type network type can be `'bipartite'` or `'unipartite'`
#' @param directed whether or not connections are directed (`T`) or symetrical (`F`) (default is set to `TRUE`)
#'
#'
#' @return an adjacency/incidence matrix (data.frame) representing the network
#'
#' @examples
#' # For unipartite network
#' data_uni <- PatientDoctorNetwork$patient_patient
#'
#' # If your network is symmetric :
#' my_mat <- edges_to_adjacency(data_uni,
#'   type = "unipartite",
#'   directed = FALSE
#' )
#' # If your network is directed :
#' my_mat <- edges_to_adjacency(data_uni,
#'   type = "unipartite",
#'   directed = TRUE
#' )
#'
#' # For bipartite network
#' data_bi <- PatientDoctorNetwork$doctor_patient
#'
#' my_mat <- edges_to_adjacency(data_bi, type = "bipartite")
#'
#' # In any case you can also use 2 columns data.frames if your network is binary.
#' binary_net <- PatientDoctorNetwork$doctor_patient[,-3]
#'
#' my_mat <- edges_to_adjacency(binary_net, type = "bipartite")
#'
#' @export
edges_to_adjacency <- function(my_list, type = c("unipartite", "bipartite"), directed = T) {
  ## Rename columns of the pair of node list by 'from', 'to' and 'value' (if needed)
  if (dim(my_list)[2] == 2) {
    names(my_list) <- c("from", "to")
  } else if (dim(my_list)[2] == 3) {
    names(my_list) <- c("from", "to", "value")
  } else {
    warning("my_list should be a data.frame of 2 or 3 columns")
    return(my_list)
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
  if (type[1] == "unipartite" & !directed) {
    mat[my_list[, 2:1]] <- ifelse(rep(dim(my_list)[2] == 2, dim(my_list)[1]),
      1, my_list[, 3]
    )
  }
  return(as.data.frame(mat))
}
