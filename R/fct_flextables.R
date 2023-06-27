
#' round_prop
#'
#' @description A fct that round a proportion vector and keep sum at one
#'
#' @return the rounded vector
#'
#' @noRd
round_prop <- function(x, digits = 2) {
  prop_vec <- round(x, digits)
  prop_vec[length(prop_vec)] <- 1 - sum(prop_vec[1:(length(prop_vec) - 1)])
  return(prop_vec)
}



#' flexBlockProp
#'
#' @description A fct that gives a nice flextable for block proportion from the sbm param
#'
#' @return The block proportions in flextable
#'
#' @noRd
flexBlockProp <- function(sbm,labels){
  if(is.bipartite(sbm)){
    row <- round_prop(sbm$blockProp$row)
    col <- round_prop(sbm$blockProp$col)
    n <- max(length(row), length(col))
    length(row) <- n
    length(col) <- n
    data_prop <- data.frame(Blocks = 1:n,
                            row = row,
                            col = col) %>%
      setNames(c("Blocks",labels[['row']],labels[['col']]))
  }else{
    nodes <- round_prop(sbm$blockProp)
    data_prop <- data.frame(Blocks = 1:length(nodes),
                            nodes = nodes) %>%
      setNames(c("Blocks",labels[['row']]))
  }
  ft <- flextable::flextable(data_prop) %>%
    flextable::set_caption(caption = "Block Proportions:") %>%
    flextable::theme_vanilla() %>%
    flextable::autofit()
  return(ft)
}


#' flexConnect
#'
#' @description A fct that gives a nice flextable for connectivity from the sbm param
#'
#' @return The connectivity in flextable
#'
#' @noRd
flexConnect <- function(sbm,labels){
  data_connect <- as.data.frame(round(sbm$connectParam$mean,2)) %>%
    setNames(1:length(.)) %>%
    dplyr::mutate(rowlabel = labels[['row']],rowNb = 1:nrow(.),.before = 1)

  ft <- flextable::flextable(data_connect) %>%
    flextable::merge_v(1) %>%
    flextable::rotate(j = 1,rotation="btlr",align = "center") %>%
    flextable::bold(j = 1:2)

  if(sbm$modelName == "gaussian"){
    ft <- flextable::add_footer_row(ft,
                                    values = c("Variance",round(sbm$connectParam$var[[1]], 3),""),
                                    colwidths = c(2,1,length(data_connect)-3)) %>%
      flextable::bold(j = 1,part = 'footer')
  }

  ft <- flextable::theme_vanilla(ft) %>%
    flextable::void(j = 1:2,part = "header") %>%
    flextable::add_header_row(values = c("","",labels[['col']]),
                              colwidths = c(1,1,length(data_connect)-2)) %>%
    flextable::align(i = 1,j=NULL,align = 'center',part = "header") %>%
    flextable::align(j=1,align = 'right',part = "footer") %>%
    flextable::set_caption(caption = "Connectivity Betweens Blocks:") %>%
    flextable::autofit()

  return(ft)
}
