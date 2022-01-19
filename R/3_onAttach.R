.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0(
    "plnr ",
    utils::packageDescription("plnr")$Version,
    "\n",
    "https://docs.sykdomspulsen.no/plnr"
  ))
}
