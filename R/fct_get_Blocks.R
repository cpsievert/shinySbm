#' getBlock generic
#'
#' @description A that return the list of names divided in the different blocks
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
getBlocks <- function(x, node_names, labels = NULL,
                      attribution = T, proportion = F) {
  UseMethod("getBlocks", object = x)
}

#' getBlock.SimpleSBM_fit method
#'
#' @description A that return the list of names divided in the different Blocks
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
getBlocks.SimpleSBM_fit <- function(x, node_names, labels = NULL,
                                    attribution = T, proportion = F) {

  if (!(attribution | proportion)) {
    attribution <- T
  }
  if(is.sbmMatrix(node_names)){
    res <- data.frame(Nodes_names = node_names$nodes_names$col)
  }else if(is.character(node_names)){
    res <- data.frame(Nodes_names = node_names)
  }else{
    stop("node_names should be a character containing nodes names or an 'sbmMatrix'")
  }

  if (attribution) {
    res$Blocks <- paste0("Block_", x$memberships)
  }
  if (proportion) {
    res <- cbind(
      res,
      x$probMemberships %>%
        as.data.frame() %>%
        setNames(paste0("Proba_block_", 1:x$nbBlocks[[1]]))
    )
  }
  return(res)
}


#' getBlock.BipartiteSBM_fit method
#'
#' @description A that return the list of names divided in the different Blocks
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
getBlocks.BipartiteSBM_fit <- function(x, node_names,
                                       labels = c(row = "row",col = "col"),
                                       attribution = T, proportion = F) {
  if (!(attribution | proportion)) {
    attribution <- T
  }
  if(is.sbmMatrix(node_names)){
    res <- list(row = data.frame(Nodes_names = node_names$nodes_names$row),
                col = data.frame(Nodes_names = node_names$nodes_names$col))
  }else if(is.list(node_names)){
    res <- list(row = data.frame(Nodes_names = node_names$row),
                col = data.frame(Nodes_names = node_names$col))
  }else{
    stop("node_names should be a list of nodes names or an 'sbmMatrix'")
  }

  if (attribution) {
    res$row$Blocks <- paste0("Block_",labels[["row"]],'_',x$memberships$row)
    res$col$Blocks <- paste0("Block_", labels[["col"]],'_',x$memberships$col)
  }
  if (proportion) {
    res$row <- cbind(
      res$row,
      x$probMemberships$row %>%
        as.data.frame() %>%
        setNames(paste0("Prop_block_", 1:x$nbBlocks[[1]]))
    )
    res$col <- cbind(
      res$col,
      x$probMemberships$col %>%
        as.data.frame() %>%
        setNames(paste0("Prop_block_", 1:x$nbBlocks[[2]]))
    )
  }
  return(res)
}
