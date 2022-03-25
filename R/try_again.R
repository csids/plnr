#' try again
#' @param x code
#' @param times Number of times to try
#' @param delay_seconds_min Number of seconds to delay on failure
#' @param delay_seconds_max Number of seconds to delay on failure
#' @param verbose Boolean. Do you want information?
#' @export
#' Adapted from function try_again from package testthat.
try_again <- function (x, times = 1, delay_seconds_min = 1, delay_seconds_max = delay_seconds_min, verbose = FALSE) {
  i <- 1
  while (i <= times) {
    err <- tryCatch(withCallingHandlers({
      x
      NULL
    }, warning = function(err) {
      if (identical(err$message, "restarting interrupted promise evaluation")) {
        if(!is.null(findRestart("muffleWarning"))){
          invokeRestart("muffleWarning")
        }
      }
    }), expectation_failure = function(err) {
      err
    }, error = function(err) {
      err
    })

    if(is.null(err)) {
      if(i>1 & verbose){
        message(i,"/",times,": Succeeded.")
      }
      return(invisible(TRUE))
    }

    if(verbose) warning(i,"/",times,": Failed", call. = FALSE, immediate. = TRUE)
    Sys.sleep(runif(1, delay_seconds_min, delay_seconds_max))
    i <- i + 1L
  }
  stop(err)
}

