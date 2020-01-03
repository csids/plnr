#' txt_progress_bar
#' @param min a
#' @param max a
#' @param initial a
#' @param char a
#' @param width a
#' @param title a
#' @param label a
#' @param style a
#' @param file a
#' @param time_start a
#' @export
txt_progress_bar <- function(min = 0, max = 1, initial = 0, char = "=", width = NA,
                             title, label, style = 1, file = "", time_start = Sys.time()) {
  if (!identical(file, "") && !(inherits(file, "connection") &&
    isOpen(file))) {
    stop("'file' must be \"\" or an open connection object")
  }
  if (!style %in% 1L:3L) {
    style <- 1
  }
  .val <- initial
  .killed <- FALSE
  .nb <- 0L
  .pc <- -1L
  nw <- nchar(char, "w")
  if (is.na(width)) {
    width <- getOption("width")
    if (style == 3L) {
      width <- width - 10L
    }
    width <- trunc(width / nw)
  }
  if (max <= min) {
    stop("must have 'max' > 'min'")
  }

  up <- function(value) {
    if (!is.finite(value) || value < min || value > max) {
      return()
    }
    .val <<- value
    nb <- round(width * (value - min) / (max - min))
    pc <- 100 * (value - min) / (max - min)

    time_now <- Sys.time()
    mins_from_start <- as.numeric(difftime(time_now, time_start, units = "mins"))
    total_time <- 100 * mins_from_start / pc
    if (is.nan(total_time) | is.infinite(total_time)) total_time <- 0
    mins_remaining <- total_time - mins_from_start

    pc <- round(pc)

    if (nb == .nb && pc == .pc) {
      return()
    }
    # cat(paste0("\r  |", strrep(" ", nw * width + 6)), file = file)
    cat(
      sprintf("\r***** %s/%s = %3d%%;  (R+E=T) %.1f + %.1f = %.1f min(s) *****", value, max, pc, mins_remaining, mins_from_start, total_time),
      file = file
    )
    utils::flush.console()
    .nb <<- nb
    .pc <<- pc
  }
  getVal <- function() .val
  kill <- function() {
    if (!.killed) {
      cat("\n", file = file)
      utils::flush.console()
      .killed <<- TRUE
    }
  }

  Sys.sleep(0.01)
  up(initial)
  structure(list(getVal = getVal, up = up, kill = kill), class = "txtProgressBar")
}
