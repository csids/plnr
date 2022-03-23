.onAttach <- function(libname, pkgname) {
  version <- tryCatch(
    utils::packageDescription("plnr", fields = "Version"),
    warning = function(w) {
      1
    }
  )

  packageStartupMessage(paste0(
    "plnr ",
    version,
    "\n",
    "https://docs.sykdomspulsen.no/plnr"
  ))
}
