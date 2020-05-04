#' Is this code run directly?
#'
#' This function determines if it is being called from within a function
#' or if it is being run directly
#' @export
is_run_directly <- function() {
  return(
    match.call(call = sys.call(sys.parent(1)))[[1]] %in%
      c(
        "is_run_directly",
        "plnr::is_run_directly",
        "plnr:::is_run_directly"
      )
  )
}
