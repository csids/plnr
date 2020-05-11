#' Gets anything
#' @param x the string that we are getting
#' @export
get_anything <- function(x){
  if(length(grep("::", x))>0) {
    parts<-strsplit(x, "::")[[1]]
    getExportedValue(parts[1], parts[2])
  } else {
    get(x)
  }
}
