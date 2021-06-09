#' easy_split
#' @param x a
#' @param size a
#' @examples
#' easy_split(letters[1:20], size_of_each_group = 3)
#' easy_split(letters[1:20], number_of_groups = 3)
#' @export
easy_split <- function(x, size_of_each_group = NULL, number_of_groups = NULL) {
  if(is.null(size_of_each_group) & is.null(number_of_groups)) stop("you must specify ONE of size_of_each_group OR number_of_groups")
  if(!is.null(size_of_each_group) & !is.null(number_of_groups)) stop("you must specify ONE of size_of_each_group OR number_of_groups")

  if(!is.null(size_of_each_group)){
    return(split(x, ceiling(seq_along(x) / size_of_each_group)))
  }

  if(!is.null(number_of_groups)){
    splitting_index <- rep(1:number_of_groups, each = ceiling(length(x)/number_of_groups))[1:length(x)]
    return(split(x, splitting_index))
  }
}
