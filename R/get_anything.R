#' Get objects with package namespace support
#'
#' This function extends `base::get()` to support package namespace scoping (e.g., "pkg::var").
#' It's particularly useful when working with package exports and namespace-qualified objects.
#'
#' @param x Character string specifying the object to retrieve. Can be either a simple
#' object name or a namespace-qualified name (e.g., "pkg::var")
#' @return The requested object
#' @examples
#' # Get a namespace-qualified object
#' plnr::get_anything("plnr::nor_covid19_cases_by_time_location")
#'
#' # Get a simple object (same as base::get)
#' x <- 1
#' get_anything("x")
#' @export
get_anything <- function(x) {
  if (length(grep("::", x)) > 0) {
    parts <- strsplit(x, "::")[[1]]
    getExportedValue(parts[1], parts[2])
  } else {
    get(x)
  }
}
