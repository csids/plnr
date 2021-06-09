#' split_equal
#' @param x a
#' @param size a
#' @export
split_equal <- function(x, size = 10) {
  split(x, ceiling(seq_along(x) / size))
}
