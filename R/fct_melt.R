#' melt_matrix : equivalent of reshape2::melt.matrix
#'
#' @description A function that return matrix value in 3 columns Var1: rownames or row number,  Var2: colnames or col number, value: value stored at matrix location
#'
#' @return matrix value in 3 columns Var1: rownames or row number,  Var2: colnames or col number, value: value stored at matrix location
#'
#' @noRd
melt_matrix <- function(data){
  . <- NULL
  if(is.null(rownames(data))){
    row_names <- 1:nrow(data)
  }else{
    row_names <- rownames(data)
  }

  if(is.null(colnames(data))){
    col_names <- 1:ncol(data)
  }else{
    col_names <- colnames(data)
  }

  data.frame(
    Var1 = rep(row_names,times = length(col_names)),
    Var2 = rep(col_names,each = length(row_names)),
    value = as.numeric(data)
  ) %>%
    `row.names<-`(1:nrow(.))

}

