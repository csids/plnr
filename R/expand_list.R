#' Create a cross product of lists
#'
#' This function creates a cross product of multiple lists, similar to `purrr::cross()`
#' but with a more convenient interface that doesn't require wrapping arguments in an
#' extra `list()`. It's useful for generating combinations of parameters for analysis.
#'
#' @param ... Named arguments, each containing a vector or list of values to combine
#' @return A list of lists, where each inner list contains one combination of values
#' from the input arguments
#' @examples
#' # Create combinations of parameters
#' combinations <- plnr::expand_list(
#'   a = 1:2,
#'   b = c("a", "b")
#' )
#' 
#' # View the combinations
#' str(combinations)
#' 
#' # Compare with purrr::cross
#' purrr::cross(list(
#'   a = 1:2,
#'   b = c("a", "b")
#' ))
#' @export
expand_list <- function(...) {
  dots <- list(...)
  purrr::cross(dots)
}
