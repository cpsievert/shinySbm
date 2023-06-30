#' is.bipartite
#'
#' @description it's a function that says if your object (graph, sbm like or pre-sbm) is a bipartite one
#'
#' @param object is a list or a result of {sbm} estimation that has to be checked whether or not is is bipartite
#'
#' @return TRUE if bipartite, FALSE if not
#'
#' @examples
#' my_net1 <- list(pop = 8, networktype = "bipartite")
#' is.bipartite(my_net1)
#'
#' my_net2 <- list(pop = 8, networktype = "unipartite")
#' is.bipartite(my_net2)
#'
#' my_sbm <- sbm::estimateBipartiteSBM(sbm::fungusTreeNetwork$fungus_tree,model = 'bernoulli')
#' is.bipartite(my_sbm)
#'
#' @export
#'
is.bipartite <- function(object) {
  UseMethod("is.bipartite", object)
}


#' is.bipartite.default
#'
#' @description it's a function that says if your object (graph, sbm like or pre-sbm) is a bipartite one
#'
#' @param object is a list that has to be checked whether or not is is bipartite
#'
#' @return TRUE if bipartite, FALSE if not
#'
#' @examples
#'
#' is.bipartite(1)
#'
#' @export
#'
is.bipartite.default <- function(object) {
  warning("object should be of class: 'list', 'SBM' or 'sbmMatrix'")
  return(FALSE)
}


#' is.bipartite.list
#'
#' @description it's a function that says if your object (graph, sbm like or pre-sbm) is a bipartite one
#'
#' @param object is a list that has to be checked whether or not is is bipartite
#'
#' @return TRUE if bipartite, FALSE if not
#'
#' @examples
#'
#' my_net1 <- list(pop = 8, networktype = "bipartite")
#' is.bipartite(my_net1)
#'
#' my_net2 <- list(pop = 8, networktype = "unipartite")
#' is.bipartite(my_net2)
#'
#' @export
#'
is.bipartite.list <- function(object) {
  any(purrr::map_lgl(object,~identical(.x,"bipartite")))
}

#' is.bipartite.SBM
#'
#' @description it's a function that says if your sbm is bipartite or not
#'
#' @param object an sbm model from {sbm} package that has to be checked whether or not is is bipartite
#'
#' @return a Boolean, TRUE for bipartite or FALSE if else
#'
#' @examples
#'
#' my_sbm <- sbm::estimateBipartiteSBM(sbm::fungusTreeNetwork$fungus_tree,model = 'bernoulli')
#' is.bipartite(my_sbm)
#'
#' @export
#'
is.bipartite.SBM <- function(object){
  if("BipartiteSBM" %in% class(object)){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

#' is.bipartite.sbmMatrix
#'
#' @description it's a function that says if your sbm is bipartite or not
#'
#' @param object an sbmMatrix  that has to be checked whether or not is is bipartite
#'
#' @return a Boolean, TRUE for bipartite or FALSE if else
#'
#' @noRd
is.bipartite.sbmMatrix <- function(object){
  if(object$type == 'bipartite'){
    return(TRUE)
  }else{
    return(FALSE)
  }
}
