#' css_big_table
#'
#' @description A fct that write a css style for my big datatable print from id and namespace
#'
#' @return css value
#'
#' @noRd
css_big_table <- function(id, ns = identity) {
  HTML(
    paste0(
      "#", ns(id), " > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody {
    transform:rotateX(180deg);
    }", "\n",
      "#", ns(id), " > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody table{
    transform:rotateX(180deg);
    }"
    )
  )
}
