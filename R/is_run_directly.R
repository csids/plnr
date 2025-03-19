#' Check if code is being run directly or from within a function
#'
#' This function determines whether code is being executed directly in the global environment
#' or from within a function call. It's particularly useful for development and debugging
#' purposes, allowing functions to behave differently when run directly versus when called
#' as part of a larger analysis plan.
#'
#' @return A logical value: `TRUE` if the code is being run directly (i.e., from the global
#' environment), `FALSE` if it's being run from within a function call
#' @examples
#' # When run directly
#' is_run_directly()  # TRUE
#'
#' # When run from within a function
#' test_fn <- function() {
#'   is_run_directly()  # FALSE
#' }
#' test_fn()
#' @export
is_run_directly <- function() {
  sys.nframe() == 1
}
