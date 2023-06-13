#' smalltable
#'
#' @description a function that show an small table
#'
#' @param data data.frame
#' @param header does it show a header
#' @title title for the table
#'
#' @return HTML for a beutifull smalltable
#'
#' @noRd
smalltable <- function(data, header = F, rownames = F, title = NULL,nb_digits = 2){
  flextable::set_flextable_defaults(digits = nb_digits)
  fp_cap <- flextable::fp_text_default(bold = T)
  tbl <- flextable::as_flextable(data,show_coltype = F)
  if(!header){
    tbl <- flextable::delete_part(tbl, part = "header")
  }
  if(!is.null(title)){
    tbl <- flextable::set_caption(tbl,caption = title,fp_p = fp_cap)
  }
  if(rownames){
    tbl <- flextable::void(tbl,j = 1, part = "header")
  }
  tbl <- flextable::border_inner(tbl) %>%
    flextable::border_outer()
  return(tbl)
}
