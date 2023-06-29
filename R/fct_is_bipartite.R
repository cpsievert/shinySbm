#' plotSbm
#'
#' @description A fct that plot a beautiful matrix from an sbm object or a network matrix it does
#' have suitable parameters to get the plots you want this is the generic function,
#' it does have one method Bipartite and one for Simple Sbm. The `fit` object need
#' to be construct by one of the `estimate***SBM` function from the `sbm` package.
#'
#' @param fit  : Sbm model of class `BipartiteSBM_fit`, `SimpleSBM_fit` or simple numeric `matrix`.
#' @param ordered : Boolean. Set \code{TRUE} if the matrix should be reordered (Default is \code{FALSE})
#' @param transpose : Boolean. Set \code{TRUE} if you want to invert columns and rows to flatten a long matrix (Default is \code{FALSE})
#' @param labels : a named list (names should be : `"col"` and `"row"`) of characters describing columns and rows component (Default is \code{NULL})
#' @param plotOptions : a list providing options. See details below.
#'
#' @details The list of parameters \code{plotOptions} for the matrix plot is
#' \itemize{
#'  \item{"showValues": }{Boolean. Set TRUE if you want to see the real values. Default value is TRUE}
#'  \item{"showPredictions": }{Boolean. Set TRUE if you want to see the predicted values. Default value is TRUE}
#'  \item{"title": }{Title in characters. Will be printed at the bottom of the matrix. Default value is NULL}
#'  \item{"colPred": }{Color of the predicted values, the small values will be more transparent. Default value is "red"}
#'  \item{"colValue": }{Color of the real values, the small values will close to white. Default value is "black"}
#' }
#'
#' @return a ggplot object corresponding to the plot
#'
#' @examples
#' data_bi <- sbm::fungusTreeNetwork$fungus_tree
#' my_sbm_bi <- sbm::estimateBipartiteSBM(data_bi)
#' plotSbm(my_sbm_bi,
#'   ordered = TRUE, transpose = TRUE,
#'   plotOptions = list(title = "An example Matrix")
#' )
#'
#' data_uni <- sbm::fungusTreeNetwork$tree_tree
#' my_sbm_uni <- sbm::estimateSimpleSBM(data_uni,model = "poisson")
#' plotSbm(my_sbm_uni, ordered = TRUE,
#'   plotOptions = list(title = "An example Matrix")
#' )
#'
#' n_col <- 100
#' n_row <- 90
#' mat <- matrix(sample(0:10,n_col*n_row,replace = TRUE),n_col,n_row)
#' plotSbm(mat, transpose = TRUE ,
#'   labels = list(col = "Columns",row = 'Rows'),
#'   plotOptions = list(colValue = 'blue')
#'   )
#'
#' @export
plotSbm <- function(fit, ordered = FALSE, transpose = FALSE, labels = NULL, plotOptions = list()) {
  UseMethod("plotSbm", fit)
}



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
