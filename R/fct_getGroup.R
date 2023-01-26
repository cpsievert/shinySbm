#' getGroup generic
#'
#' @description A that return the list of names divided in the different groups
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
getGroupNames <- function(sbmObject, data) {
  UseMethod("getGroupNames", object = sbmObject)
}

#' getGroup.SimpleSBM_fit method
#'
#' @description A that return the list of names divided in the different groups
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
getGroupNames.SimpleSBM_fit <- function(sbmObject, data) {
  list_group <- sapply(1:sbmObject$nbBlocks, function(i) {
    indices <- sbmObject$indMemberships[, i] == 1
    data$nodes_names$col[indices]
  })
  list_group$type <- 'unipartite'
  return(list_group)
}


#' getGroup.BipartiteSBM_fit method
#'
#' @description A that return the list of names divided in the different groups
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
getGroupNames.BipartiteSBM_fit <- function(sbmObject, data) {
  list_group <- lapply(c("row", "col"), function(margin) {
    sapply(1:as.list(sbmObject$nbBlocks)[[margin]], function(i) {
      indices <- sbmObject$indMemberships[[margin]][, i] == 1
      if (margin == "col") {
        return(data$nodes_names$col[indices])
      } else {
        return(data$nodes_names$row[indices])
      }
    })
  })
  names(list_group) <- c("row", "col")
  list_group$type <- 'bipartite'
  return(list_group)
}
