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
  results <- list()
  results$listed_groups <- sapply(1:sbmObject$nbBlocks, function(i) {
    indices <- sbmObject$indMemberships[, i] == 1
    data$nodes_names$col[indices]
  })
  results$type <- 'unipartite'
  return(results)
}


#' getGroup.BipartiteSBM_fit method
#'
#' @description A that return the list of names divided in the different groups
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
getGroupNames.BipartiteSBM_fit <- function(sbmObject, data) {
  results <- list()
  results$listed_groups <- lapply(c("row", "col"), function(margin) {
    sapply(1:as.list(sbmObject$nbBlocks)[[margin]], function(i) {
      indices <- sbmObject$indMemberships[[margin]][, i] == 1
      if (margin == "col") {
        return(data$nodes_names$col[indices])
      } else {
        return(data$nodes_names$row[indices])
      }
    })
  })
  names(results$listed_groups) <- c("row", "col")
  results$type <- 'bipartite'
  return(results)
}
