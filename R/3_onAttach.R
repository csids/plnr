#' @import data.table ggplot2
#' @importFrom magrittr %>%
.onAttach <- function(libname, pkgname) {
  .onAttach <- function(libname, pkgname) {
    packageStartupMessage(paste(
      "plnr",
      utils::packageDescription("plnr")$Version,
      "https://folkehelseinstituttet.github.io/plnr"
    ))
  }
}
