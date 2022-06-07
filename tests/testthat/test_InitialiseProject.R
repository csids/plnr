test_that("testing error if <2 arguments", {
  p <- Plan$new()
  p$add_data(fn = function() {
    3
  }, name = "ok")

  analyses <- data.frame(x = c(1:5), y = c(11:15))

  p$add_analysis_from_df(fn = sum, df = analyses)

  testthat::expect_error(
    p$run_all()
  )
})

test_that("test len()", {
  p <- Plan$new()
  p$add_data(fn = function() {
    3
  }, name = "ok")

  analyses <- data.frame(x = c(1:50), y = c(1:50))

  fn <- function(data, argset) {
    # Sys.sleep(1)
    return(1)
  }

  p$add_analysis_from_df(fn = fn, df = analyses)
  # p$run_all_progress()
  testthat::expect_equal(
    p$x_length(),
    50
  )
})

test_that("test add_analysis_from_l", {
  p <- Plan$new()
  p$add_data(fn = function() {
    3
  }, name = "ok")

  analyses <- expand_list(x = c(1:5), y = c(1:5), z = list(1:2))

  fn <- function(data, argset) {
    # Sys.sleep(1)
    return(1)
  }

  p$add_analysis_from_list(fn = fn, l = analyses)
  # p$run_all_progress()
  testthat::expect_equal(
    p$x_length(),
    25
  )
})

test_that("test run_one", {
  p <- Plan$new()
  p$add_data(
    name = "ok",
    fn = function() {
      3
    }
  )

  analyses <- data.frame(x = c(1:5), y = c(11:15))

  fn <- function(data, argset) {
    return(1)
  }

  p$add_analysis_from_df(fn = fn, df = analyses)

  testthat::expect_equal(
    p$run_one(1),
    1
  )
})

test_that("see if dots work", {
  p <- Plan$new()
  p$add_data(fn = function() {
    3
  }, name = "ok")

  analyses <- data.frame(x = c(1:5), y = c(11:15))

  fn <- function(data, argset, hello) {
    return(hello)
  }

  p$add_analysis(fn = fn, an_argument = 7)


  testthat::expect_equal(
    p$run_one(1, hello = 3),
    3
  )
})

test_that("parallel", {
  # p <- Plan$new()
  # p$data_add(fn = function(){3}, name = "ok")
  #
  # analyses <- data.frame(x=c(1:100), y=c(1:100))
  #
  # fn_analysis <- function(data, arg){
  #   Sys.sleep(1)
  # }
  #
  # p$analysis_add_from_df(fn = fn_analysis, df = analyses)
  #
  # a <- Sys.time()
  # run_all_parallel(p, cores = 10, future.chunk.size = NULL)
  # b <- Sys.time()
  # b-a

  testthat::expect_equal(
    5,
    5
  )
})

test_that("fn_name in data.frame", {
  df <- data.frame(a = c(1:2), fn_name = "test_action_fn", stringsAsFactors = FALSE)
  p <- Plan$new()
  p$add_analysis_from_df(df = df)
  x <- p$run_all()

  testthat::expect_equal(
    length(x),
    2
  )
})
