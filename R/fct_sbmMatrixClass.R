#' buildSbmMatrix
#'
#' @description A fct that build an S3 object from a data.frame or a matrix
#' obj is the initial network matrix, and argument in ... are the covariables on the link between nodes
#' ... argument should be named covariables on the interactions, so it's should be matrices of equal dimension than the network
#'
#' @param obj,...,Col_names,row_names
#' `Obj` can be data.frame or a matrix
#' `...` are covariables, they can be named or not : they should dataframes or matrixes of the same dimension than the network one
#' `col_names` (respectively. `row.names`) should the node names in columns (resp. rows) of the network matrix
#'
#'
#' @return the return value is a list of sbmMatrix class containing all the necessary information :
#' 1 - The matrix of adjacency (class :  matrix)
#' 2 - The row and col names : list of two character vectors containing the nodes names
#' 3 - The covariables : stored in a named list of matrices
#' 4 - The type of network : 'bipartite' or 'unipartite'
#' 5 - The supposed density upon the data adjacency matrix.
#'
#' @noRd
buildSbmMatrix <- function(obj, ..., col_names = NULL, row_names = NULL){
  if(is.sbmMatrix(obj)){
    message("obj is already an sbmMatrix")
    return(obj)
  }
  message("===========================\n  COMPILATION INFORMATION  \n===========================")
  # Section d'analyse d'obj
  if(is.matrix(obj)|is.data.frame(obj)){
    if(is.matrix(obj)){
      col = character(0)
      row = character(0)
    }else{
      message("Nodes names are extrated from your table")
      col = colnames(obj)
      row = row.names(obj)
    }
    if(dim(obj)[1]==dim(obj)[2]){
      default.type <- "unipartite"
    }else{
      default.type <- "bipartite"
    }
    message("Network is considered ",default.type)
    matObj <- as.matrix(obj)
    if(all(matObj==round(matObj))){
      if(all(matObj %in% c(0,1))){
        expected.law <- "bernoulli"
      }else{
        expected.law <- "poisson"
      }
    }else{
      expected.law <- "gaussian"
    }
    message("Default density is set to ",expected.law,"'s law.")
    # Section d'analyse des covariables
    cat('\n')
    list_cov <- list(...)
    # Set the names of covariables
    if(length(list_cov)!=0){
      good_covar <- sapply(list_cov,function(x,dimbase=dim(matObj))
        return(is.matrix(x)|is.data.frame(x)&&all(dim(x)==dimbase)))
      if(length(which(!good_covar))!=0){
        list_cov <- list_cov[which(good_covar)]
        warning("Covariable ",paste(paste0("n ",which(!good_covar)),collapse=', '),
                " deleted because dimensions are wrong,\n  Covaribles should be data.frames or matrixes of the same dimention than the network matrix.")
      }
      if(length(which(good_covar))!=0){
        if(is.null(names(list_cov))){
          names(list_cov) <- paste0('covar',1:length(list_cov))
        }else{
          list_unamed <- names(list_cov)==""
          names(list_cov)[list_unamed] <- paste0('covar',which(list_unamed))
        }
        list_cov <- sapply(list_cov,as.matrix,simplify = F)
      }else{
        list_cov <- NULL
      }
    }else{
      list_cov <- NULL
    }
    if(!is.null(col_names)){
      col = as.character(col_names)
    }
    if(!is.null(row_names)){
      row = as.character(row_names)
    }
    my_sbm_object <- structure(list(matrix = matObj, nodes_names = list(col = col,row = row),
                                    covar = list_cov, type = default.type, law = expected.law),class = "sbmMatrix")
    is.sbmMatrix(my_sbm_object,force_stop = T)
    return(my_sbm_object)
  }else{
    stop("obj is a '",class(obj),"' object.\n Should be a 'data.frame' or 'matrix' object.")
  }
}



#' is.sbmMatrix
#'
#' @description A fct that analyse an supposed sbmMatrix and tell if it's correctly set.
#'
#' @param my_sbm_object,force_stop
#' `my_sbm_object` is an sbmMatrix
#' `force_stop` should the function set an error or a warning when there is a problem in the object
#'
#' @return `TRUE` if the format is good, `FALSE` if not
#'
#' @noRd
is.sbmMatrix <- function(my_sbm_object, force_stop = FALSE){
  if(any(class(my_sbm_object)=='sbmMatrix')){
    if(is.list(my_sbm_object)){
      conforme <- rep(F,4)
      dimbase <- dim(my_sbm_object)
      conforme[1] <- is.matrix(my_sbm_object$matrix)
      conforme[2] <- length(my_sbm_object$nodes_names$row)==dimbase[1] & length(my_sbm_object$nodes_names$col)==dimbase[2]
      conforme[2] <- conforme[2] & is.character(my_sbm_object$nodes_names$row) &
        is.character(my_sbm_object$nodes_names$col)
      if(identical(my_sbm_object$nodes_names$col,character(0)) &
         identical(my_sbm_object$nodes_names$row,character(0))){
        if(force_stop){
          warning("You didn't give any nodes names.")
        }

        conforme[2] <- T
      }
      conforme[3] <- conforme[3] | is.null(my_sbm_object$covar)
      conforme[3] <- conforme[3] | all(sapply(my_sbm_object$covar,function(x,dimbase=dimbase)
        return(is.matrix(x)|is.data.frame(x) && all(dim(x)==dimbase))))
      conforme[4] <- my_sbm_object$type %in% c('bipartite','unipartite') &
        my_sbm_object$law %in% c('poisson','gaussian','bernoulli')
      if(force_stop){
        if(!all(conforme)){
          warning("The is a problem in your sbmMatrix. It can't be read. You should build it with :\nbuildSbmMatrix(obj,...) ")
        }
        if(!conforme[1]){
          stop("Network matrix has the wrong format.")
        }
        if(!conforme[2]){
          stop("Nodes names aren't correctly set.")
        }
        if(!conforme[3]){
          stop("Covariables aren't correct.")
        }
        if(!conforme[4]){
          stop("type of network or density's law can't be read.")
        }
      }
      return(all(conforme))
    }else{
      if(force_stop){
        stop("my_sbm_object isn't a list")
      }
      return(F)
    }
  }else{
    if(force_stop){
      stop("my_sbm_object doesn't have the class : 'sbmMatrix'")
    }
    return(F)
  }
}



#' print.sbmMatrix
#'
#' @description print method for sbmMatrix object
#'
#' @param x,show_matrix,resume_table,show_covar,...
#' `x` an sbmMatrix
#' `show_matrix` a boolean, should it show the matrix ?
#' `resume_table` a boolean, should it shorten the matrices dimensions ? (5 columns and 10 lines)
#' `show_covar` a boolean, should it show the covariable's matrices ?
#' `...` form print `.Primitive`
#'
#' @return No Values returned
#'
#' @noRd
print.sbmMatrix <- function(x, show_matrix = T, resume_table = T, show_covar = F,...){
  dimbase <- dim(x)
  if(resume_table){
    if(dimbase[1]>10){
      index_row <- 1:10
    }else{
      index_row <- 1:dimbase[1]
    }
    if(dimbase[2]>5){
      index_col <- 1:5
    }else{
      index_col <- 1:dimbase[2]
    }
  }else{
    index_row <- 1:dimbase[1]
    index_col <- 1:dimbase[2]
  }
  if(!is.sbmMatrix(x)){
    warning("x object got the sbmMatrix class but got a wrong format.")
    print.default(x)
  }else{
    cat("==========================\n SBM MATRIX INFORMATION :  \n==========================\n\n")
    cat("sbmMatrix of an", x$type, "network. The expected law upon this matrix is a poisson density.\n")
    if(x$type == "unipartite"){
      cat("The network has",dimbase[1],"nodes.")
    }else if(x$type == 'bipartite'){
      cat("The network has",dimbase[1],"row nodes &",dimbase[2],"column nodes.")
    }
    if(identical(x$nodes_names$col,character(0)) &
       identical(x$nodes_names$row,character(0))){
      cat(" The nodes names aren't registered.\n")
    }
    if(show_matrix){
      cat('\n')
      print(x$matrix[index_row,index_col])
    }
    if(is.null(x$covar)){
      cat("\nThere is no covariables.\n\n")
    }else{
      cat("\nThere is",length(x$covar)," covariables : ")
      if(!show_covar){
        cat(paste(names(x$covar),collapse = ', '),'\n')
      }else{
        cat('\n\n')
        for(i in 1:length(x$covar)){
          print(names(x$covar)[i])
          print(x$covar[[i]][index_row,index_col])
          cat('\n')
        }
      }
    }
  }
}

#' as.data.frame.sbmMatrix
#'
#' @description as.data.frame method for sbmMatrix object
#'
#' @param x,row.names,optional,...
#' `x` is an sbmMatrix
#' `row.names`,`Optional`,`...` are arguments of `as.data.frame.default`
#'
#' @return a data.frame object that contain the values of the main matrix.
#' If the is names in the sbmMatrix object they will be put as names in the dataframe.
#'
#' @noRd
as.data.frame.sbmMatrix <- function(x, row.names = NULL, optional = FALSE, ...){
  table <- data.frame(x$matrix)
  if(!identical(x$nodes_names$col,character(0))){
    names(table) <- x$nodes_names$col
  }
  if(!identical(x$nodes_names$row,character(0))){
    row.names(table) <- x$nodes_names$row
  }
  return(table)
}

#' as.matrix.sbmMatrix
#'
#' @description as.matrix method for sbmMatrix object
#'
#' @param x,...
#' `x` is an sbmMatrix
#' `...` is an arguments of `as.matrix.default`
#'
#' @return a matrix object that contain the values of the main matrix.
#' If the is names in the sbmMatrix object they will be put as names in the dataframe.
#'
#' @noRd
as.matrix.sbmMatrix <- function(x, ...){
  matrix <- x$matrix
  if(!identical(x$nodes_names$col,character(0))){
    colnames(matrix) <- x$nodes_names$col
  }
  if(!identical(x$nodes_names$row,character(0))){
    rownames(matrix) <- x$nodes_names$row
  }
  return(matrix)
}


#' dim.sbmMatrix
#'
#' @description dim method for sbmMatrix object
#'
#' @param x is an sbmMatrix
#'
#' @return The dimension of the network matrix
#'
#' @noRd
dim.sbmMatrix <- function(x){
  return(dim(x$matrix))
}

#' cov
#'
#' @description return out the covariables of an sbmMatrix
#'
#' @param x is an sbmMatrix
#'
#' @return the list of covariables in x
#'
#' @noRd
cov <- function(x){
  if(is.sbmMatrix(x)){
    return(x$covar)
  }
  stop("x isn't an sbmMatrix.\n Check the problem with is.sbmMatrix(x, force_stop = T)")
}


