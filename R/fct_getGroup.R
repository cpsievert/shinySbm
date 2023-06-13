#' getGroup generic
#'
#' @description A that return the list of names divided in the different groups
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
getGroups <- function(sbmObject, sbmData, attribution = T,
                      proportion = F, labels = NULL) {
  UseMethod("getGroups", object = sbmObject)
}

#' getGroup.SimpleSBM_fit method
#'
#' @description A that return the list of names divided in the different groups
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
getGroups.SimpleSBM_fit <- function(sbmObject, sbmData, attribution = T,
                                    proportion = F, labels = NULL) {

  if (!(attribution | proportion)) {
    attribution <- T
  }
  res <- data.frame(Nodes_names = sbmData$nodes_names$col)
  if (attribution) {
    res$Groups <- paste0("Group_", sbmObject$memberships)
  }
  if (proportion) {
    res <- cbind(
      res,
      sbmObject$probMemberships %>%
        as.data.frame() %>%
        setNames(paste0("Proba_group_", 1:sbmObject$nbBlocks[[1]]))
    )
  }
  return(res)
}


#' getGroup.BipartiteSBM_fit method
#'
#' @description A that return the list of names divided in the different groups
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
getGroups.BipartiteSBM_fit <- function(sbmObject, sbmData, attribution = T,
                                       proportion = F, labels = list(row = "row",
                                                                     col = "col")) {
  if (!(attribution | proportion)) {
    attribution <- T
  }
  res <- list(row = data.frame(Nodes_names = sbmData$nodes_names$row),
              col = data.frame(Nodes_names = sbmData$nodes_names$col))
  if (attribution) {
    res$row$Groups <- paste0("Group_",labels$row,'_',sbmObject$memberships$row)
    res$col$Groups <- paste0("Group_", labels$col,'_',sbmObject$memberships$col)
  }
  if (proportion) {
    res$row <- cbind(
      res$row,
      sbmObject$probMemberships$row %>%
        as.data.frame() %>%
        setNames(paste0("Prop_group_", 1:sbmObject$nbBlocks[[1]]))
    )
    res$col <- cbind(
      res$col,
      sbmObject$probMemberships$col %>%
        as.data.frame() %>%
        setNames(paste0("Prop_group_", 1:sbmObject$nbBlocks[[2]]))
    )
  }
  return(res)
}
