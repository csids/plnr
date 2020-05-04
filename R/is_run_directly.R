#' Is this code run directly?
#'
#' This function determines if it is being called from within a function
#' or if it is being run directly
#' @export
is_run_directly <- function() {
  sys.nframe() == 1
}
