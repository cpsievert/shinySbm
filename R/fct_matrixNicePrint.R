#' show_table
#'
#' @description print table correctly even with long names
#'
#' @param data,str_len=10,tbl_len=25,tbl_wid=8,show_dim=T,drop=T
#' `data` table
#' `str_len` max names length
#' `tbl_len` max table length
#' `tbl_wid` max table width
#' `show_dim` whether or not to show the dimension of table
#'
#' @return fit better in the verbatim plot
#'
#' @noRd
show_table <- function(data, str_len = 10, tbl_len = 25, tbl_wid = 8, show_dim = T){
  cols <- colnames(data)
  rows <- rownames(data)
  short_cols <- ifelse(stringr::str_length(cols) > str_len,
                       paste0(substr(cols, 1, str_len-3),'...'),
                       cols)
  short_rows <- ifelse(stringr::str_length(rows) > str_len,
                       paste0(substr(rows, 1, str_len-3),'...'),
                       rows)
  if(any(duplicated(short_rows))){
    table <- as.matrix(data)
  }else{
    table <- data
  }

  colnames(table) <- short_cols
  rownames(table) <- short_rows

  if(dim(data)[1] > tbl_len){
    table <- table[1:tbl_len,]
  }
  if(dim(data)[2]>tbl_wid){
    table <- table[,1:tbl_wid]
  }
  if(show_dim){
    cat("Data dimension :",dim(data)[1],'rows x',dim(data)[2],'columns\n\n')
  }
  print(table)
}
