#' Expand (cross) lists
#'
#' The same as purrr::cross, but doesn't require an extra list()
#' @param ... Dots
#' @examples
#' plnr::expand_list(
#'   a = 1:2,
#'   b = c("a", "b")
#' )
#' @export
expand_list <- function(...) {
  dots <- list(...)
  purrr::cross(dots)
}
