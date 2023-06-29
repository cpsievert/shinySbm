#' is.bipartite
#'
#' @description it's a function that says if your object (graph, sbm like or pre-sbm) is a bipartite one
#'
#' @param object is the object of wich you want to know the propreties
#'
#' @return TRUE if bipartite, FALSE if not
#'
#' @noRd
#'
is.bipartite <- function(object) {
  UseMethod("is.bipartite", object)
}


#' is.bipartite.default
#'
#' @description it's a function that says if your object (graph, sbm like or pre-sbm) is a bipartite one
#'
#' @param object is the object of wich you want to know the propreties
#'
#' @return TRUE if bipartite, FALSE if not
#'
#' @noRd
#'
is.bipartite.default <- function(object) {
  warning("object should be of class: 'list', 'SBM' or 'sbmMatrix'")
  return(FALSE)
}


#' is.bipartite.list
#'
#' @description it's a function that says if your object (graph, sbm like or pre-sbm) is a bipartite one
#'
#' @param object is the object of wich you want to know the propreties
#'
#' @return TRUE if bipartite, FALSE if not
#'
#' @noRd
#'
is.bipartite.list <- function(object) {
  any(purrr::map_lgl(object,~identical(.x,"bipartite")))
}

#' is.bipartite.SBM
#'
#' @description it's a function that says if your sbm is bipartite or not
#'
#' @param sbm an sbm model from {sbm} package
#'
#' @return a Boolean, TRUE for bipartite or FALSE if else
#'
#' @noRd
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
#' @param sbm an sbm model from {sbm} package
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
