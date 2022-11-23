#' Gets anything (including with package scoping)
#'
#' base::get does not work with package scoping (e.g. get("pkg::var")).
#' plnr::get_anything works with package scoping.
#' @param x the string that we are getting
#' @examples
#' plnr::get_anything("plnr::nor_covid19_cases_by_time_location")
#' @export
get_anything <- function(x) {
  if (length(grep("::", x)) > 0) {
    parts <- strsplit(x, "::")[[1]]
    getExportedValue(parts[1], parts[2])
  } else {
    get(x)
  }
}
