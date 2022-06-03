#' Expand (cross) lists
#' The same as purrr::cross, but doesn't require an extra list()
#' @param ... Dots
#' @export
expand_list <- function(...) {
  dots <- list(...)
  purrr::cross(dots)
}
