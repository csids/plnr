#' Example action function for demonstrating analysis structure
#'
#' This function serves as an example of how to structure an action function
#' for use with the `Plan` class. It simply prints the names of the data
#' and argset components it receives.
#'
#' @param data A named list containing the datasets for the analysis
#' @param argset A named list containing the arguments for the analysis
#' @return NULL, prints information about the input data and argset
#' @examples
#' # Create a new plan
#' p <- plnr::Plan$new()
#'
#' # Add example data
#' p$add_data("covid_data", fn_name = "plnr::example_data_fn_nor_covid19_cases_by_time_location")
#'
#' # Create batch of argsets
#' batch_argset_list <- list(
#'   list(name = "analysis_1", var_1 = 1, var_2 = "i"),
#'   list(name = "analysis_2", var_1 = 2, var_2 = "j"),
#'   list(name = "analysis_3", var_1 = 3, var_2 = "k")
#' )
#'
#' # Add analyses to plan
#' p$add_analysis_from_list(
#'   fn_name = "plnr::example_action_fn",
#'   l = batch_argset_list
#' )
#'
#' # View argsets and run example
#' p$get_argsets_as_dt()
#' p$run_one("analysis_1")
#' @export
example_action_fn <- function(data, argset) {
  print("Data given:")
  print(names(data))
  print("Argset given:")
  print(names(argset))
}

#' Test action function that returns a constant value
#'
#' A simple test function that always returns 1, useful for testing
#' the Plan framework's functionality.
#'
#' @param data A named list containing the datasets (unused in this example)
#' @param argset A named list containing the arguments (unused in this example)
#' @return The integer 1
#' @export
test_action_fn <- function(data, argset) {
  return(1)
}

#' Generate a UUID
#'
#' Internal function to generate a unique identifier using the uuid package.
#'
#' @return A character string containing a UUID
#' @keywords internal
uuid_generator <- function() {
  uuid::UUIDgenerate()
}

#' Generate a hash of an object
#'
#' Internal function to create a hash of an object using the digest package.
#'
#' @param x The object to hash
#' @return A character string containing the hash
#' @keywords internal
hash_it <- function(x) {
  digest::digest(x)
}

#' R6 Class for Planning and Executing Analyses
#'
#' @description
#' The `Plan` class provides a framework for organizing and executing multiple analyses
#' on one or more datasets. It enforces a structured approach to analysis by:
#'
#' 1. **Data Management**:
#'    - Loading data once and reusing across analyses
#'    - Separating data cleaning from analysis
#'    - Providing hash-based tracking of data changes
#'
#' 2. **Analysis Structure**:
#'    - Requiring all analyses to use the same data sources
#'    - Standardizing analysis functions to accept only `data` and `argset` parameters
#'    - Organizing analyses into clear, maintainable plans
#'
#' 3. **Execution Control**:
#'    - Supporting both single-function and multi-function analysis plans
#'    - Providing flexible execution options (sequential or parallel)
#'    - Including built-in debugging tools
#'
#' @details
#' The framework uses three main concepts:
#'
#' - **Argset**: A named list containing a set of arguments for an analysis
#' - **Analysis**: A combination of one argset and one action function
#' - **Plan**: A container that holds one data pull and a list of analyses
#'
#' @import data.table
#' @import R6
#' @import foreach
#' @export
Plan <- R6::R6Class(
  "Plan",
  portable = FALSE,
  cloneable = TRUE,
  public = list(
    #' @field analyses List of analyses, each containing an argset and action function
    analyses = list(),

    #' @description Create a new Plan instance
    #' @param verbose Logical, whether to show verbose output. Defaults to TRUE in interactive mode
    #' or when config$force_verbose is TRUE
    #' @param use_foreach Logical, whether to use foreach for parallel processing.
    #' NULL = program decides, FALSE = use loop, TRUE = use foreach
    #' @return A new Plan instance
    initialize = function(verbose = interactive() | config$force_verbose, use_foreach = FALSE) {
      private$verbose <- verbose
      private$use_foreach <- use_foreach
    },

    #' @description Add a new dataset to the plan
    #' @param name Character string, name of the dataset
    #' @param fn Function that returns the dataset (optional)
    #' @param fn_name Character string, name of a function that returns the dataset (optional)
    #' @param direct Direct dataset object (optional)
    #' @return NULL, modifies the plan in place
    #' @examples
    #' p <- plnr::Plan$new()
    #'
    #' # Add data using a function
    #' data_fn <- function() { return(plnr::nor_covid19_cases_by_time_location) }
    #' p$add_data("data_1", fn = data_fn)
    #'
    #' # Add data using a function name
    #' p$add_data("data_2", fn_name = "plnr::example_data_fn_nor_covid19_cases_by_time_location")
    #'
    #' # Add data directly
    #' p$add_data("data_3", direct = plnr::nor_covid19_cases_by_time_location)
    #'
    #' # View added data
    #' p$get_data()
    add_data = function(name, fn = NULL, fn_name = NULL, direct = NULL) {
      stopifnot(is.null(fn) | is.function(fn))
      stopifnot(is.null(fn_name) | is.character(fn_name))

      private$data[[length(private$data) + 1]] <<- list(
        fn = fn,
        fn_name = fn_name,
        direct = direct,
        name = name
      )
    },

    #' @description Add a new argset to the plan
    #' @param name Character string, name of the argset (defaults to a UUID)
    #' @param ... Named arguments that will comprise the argset
    #' @return NULL, modifies the plan in place
    #' @examples
    #' p <- plnr::Plan$new()
    #'
    #' # Add argsets with different arguments
    #' p$add_argset("argset_1", var_1 = 3, var_b = "hello")
    #' p$add_argset("argset_2", var_1 = 8, var_c = "hello2")
    #'
    #' # View added argsets
    #' p$get_argsets_as_dt()
    add_argset = function(name = uuid::UUIDgenerate(), ...) {
      if (is.null(analyses[[name]])) analyses[[name]] <- list()

      dots <- list(...)
      analyses[[name]][["argset"]] <<- dots
    },

    #' @description Add multiple argsets from a data frame
    #' @param df Data frame where each row represents a new argset
    #' @return NULL, modifies the plan in place
    #' @examples
    #' p <- plnr::Plan$new()
    #'
    #' # Create data frame of argsets
    #' batch_argset_df <- data.frame(
    #'   name = c("a", "b", "c"),
    #'   var_1 = c(1, 2, 3),
    #'   var_2 = c("i", "j", "k")
    #' )
    #'
    #' # Add argsets from data frame
    #' p$add_argset_from_df(batch_argset_df)
    #'
    #' # View added argsets
    #' p$get_argsets_as_dt()
    add_argset_from_df = function(df) {
      df <- as.data.frame(df)
      for (i in 1:nrow(df)) {
        argset <- df[i, ]
        do.call(self$add_argset, argset)
      }
    },

    #' @description Add multiple argsets from a list
    #' @param l List of lists, where each inner list represents a new argset
    #' @return NULL, modifies the plan in place
    #' @examples
    #' p <- plnr::Plan$new()
    #'
    #' # Create list of argsets
    #' batch_argset_list <- list(
    #'   list(name = "a", var_1 = 1, var_2 = "i"),
    #'   list(name = "b", var_1 = 2, var_2 = "j"),
    #'   list(name = "c", var_1 = 3, var_2 = "k")
    #' )
    #'
    #' # Add argsets from list
    #' p$add_argset_from_list(batch_argset_list)
    #'
    #' # View added argsets
    #' p$get_argsets_as_dt()
    add_argset_from_list = function(l) {
      for (i in seq_along(l)) {
        argset <- l[[i]]
        do.call(self$add_argset, argset)
      }
    },

    #' @description Add a new analysis to the plan
    #' @param name Character string, name of the analysis (defaults to a UUID)
    #' @param fn Function to use for the analysis (optional)
    #' @param fn_name Character string, name of the function to use (optional)
    #' @param ... Additional arguments to be added to the argset
    #' @return NULL, modifies the plan in place
    #' @examples
    #' p <- plnr::Plan$new()
    #'
    #' # Add example data
    #' p$add_data("covid_data", fn_name = "plnr::example_data_fn_nor_covid19_cases_by_time_location")
    #'
    #' # Add analysis
    #' p$add_analysis(
    #'   name = "analysis_1",
    #'   fn_name = "plnr::example_action_fn"
    #' )
    #'
    #' # View argsets and run analysis
    #' p$get_argsets_as_dt()
    #' p$run_one("analysis_1")
    add_analysis = function(name = uuid::UUIDgenerate(), fn = NULL, fn_name = NULL, ...) {
      stopifnot(is.null(fn) | is.function(fn))
      stopifnot(is.null(fn_name) | is.character(fn_name))

      if (is.null(analyses[[name]])) analyses[[name]] <- list()

      dots <- list(...)
      analyses[[name]] <<- list(fn = fn, fn_name = fn_name)
      analyses[[name]][["argset"]] <<- dots
    },

    #' @description Add multiple analyses from a data frame
    #' @param fn Function to use for all analyses (optional)
    #' @param fn_name Character string, name of the function to use (optional)
    #' @param df Data frame where each row represents a new analysis
    #' @return NULL, modifies the plan in place
    #' @examples
    #' p <- plnr::Plan$new()
    #'
    #' # Add example data
    #' p$add_data("covid_data", fn_name = "plnr::example_data_fn_nor_covid19_cases_by_time_location")
    #'
    #' # Create data frame of analyses
    #' batch_argset_df <- data.frame(
    #'   name = c("a", "b", "c"),
    #'   var_1 = c(1, 2, 3),
    #'   var_2 = c("i", "j", "k")
    #' )
    #'
    #' # Add analyses from data frame
    #' p$add_analysis_from_df(
    #'   fn_name = "plnr::example_action_fn",
    #'   df = batch_argset_df
    #' )
    #'
    #' # View argsets and run example
    #' p$get_argsets_as_dt()
    #' p$run_one(1)
    add_analysis_from_df = function(fn = NULL, fn_name = NULL, df) {
      stopifnot(is.null(fn) | is.function(fn) | "fn_name" %in% names(df))
      stopifnot(is.null(fn_name) | is.character(fn_name))

      df <- as.data.frame(df)
      for (i in 1:nrow(df)) {
        argset <- df[i, ]
        argset$fn <- fn
        if (!"fn_name" %in% names(df)) argset$fn_name <- fn_name
        do.call(add_analysis, argset)
      }
    },

    #' @description Add multiple analyses from a list
    #' @param fn Function to use for all analyses (optional)
    #' @param fn_name Character string, name of the function to use (optional)
    #' @param l List of lists, where each inner list represents a new analysis
    #' @return NULL, modifies the plan in place
    #' @examples
    #' p <- plnr::Plan$new()
    #'
    #' # Add example data
    #' p$add_data("covid_data", fn_name = "plnr::example_data_fn_nor_covid19_cases_by_time_location")
    #'
    #' # Create list of analyses
    #' batch_argset_list <- list(
    #'   list(name = "analysis_1", var_1 = 1, var_2 = "i"),
    #'   list(name = "analysis_2", var_1 = 2, var_2 = "j"),
    #'   list(name = "analysis_3", var_1 = 3, var_2 = "k")
    #' )
    #'
    #' # Add analyses from list
    #' p$add_analysis_from_list(
    #'   fn_name = "plnr::example_action_fn",
    #'   l = batch_argset_list
    #' )
    #'
    #' # View argsets and run example
    #' p$get_argsets_as_dt()
    #' p$run_one("analysis_1")
    add_analysis_from_list = function(fn = NULL, fn_name = NULL, l) {
      stopifnot(is.null(fn) | is.function(fn))
      stopifnot(is.null(fn_name) | is.character(fn_name))

      for (i in seq_along(l)) {
        argset <- l[[i]]
        argset$fn <- fn
        if (!"fn_name" %in% names(df)) argset$fn_name <- fn_name
        do.call(add_analysis, argset)
      }
    },

    #' @description Applies an action function to all the argsets
    #' @param fn Action function.
    #' @param fn_name Action function name.
    #' p <- plnr::Plan$new()
    #' p$add_data("covid_data", fn_name = "plnr::example_data_fn_nor_covid19_cases_by_time_location")
    #' batch_argset_list <- list(
    #'   list(name = "analysis_1", var_1 = 1, var_2 = "i"),
    #'   list(name = "analysis_2", var_1 = 2, var_2 = "j"),
    #'   list(name = "analysis_3", var_1 = 3, var_2 = "k")
    #' )
    #' p$add_argset_from_list(
    #'   fn_name = "plnr::example_action_fn",
    #'   l = batch_argset_list
    #' )
    #' p$get_argsets_as_dt()
    #' p$apply_action_fn_to_all_argsets(fn_name = "plnr::example_action_fn")
    #' p$run_one("analysis_1")
    apply_action_fn_to_all_argsets = function(fn = NULL, fn_name = NULL) {
      stopifnot(is.null(fn) | is.function(fn))
      stopifnot(is.null(fn_name) | is.character(fn_name))

      for (i in seq_along(analyses)) {
        analyses[[i]]$fn <<- fn
        analyses[[i]]$fn_name <<- fn_name
      }
    },
    #' @description Deprecated. Use apply_action_fn_to_all_argsets.
    #' @param fn Action function.
    #' @param fn_name Action function name.
    apply_analysis_fn_to_all = function(fn = NULL, fn_name = NULL) {
      .Deprecated("apply_action_fn_to_all_argsets")
      self$apply_action_fn_to_all_argsets(fn = fn, fn_name = fn_name)
    },

    #' @description
    #' Number of analyses in the plan.
    x_length = function() {
      length(self$analyses)
    },

    #' @description
    #' Generate a regular sequence from 1 to the length of the analyses in the plan.
    x_seq_along = function() {
      base::seq_along(self$analyses)
    },

    #' @description
    #' Set an internal progress bar
    #' @param pb Progress bar.
    set_progress = function(pb) {
      private$pb_progress <- pb
    },

    #' @description
    #' Set an internal progressor progress bar
    #' @param pb progressor progress bar.
    set_progressor = function(pb) {
      private$pb_progressor <- pb
    },

    #' @description
    #' Set verbose flag
    #' @param x Boolean.
    set_verbose = function(x) {
      private$verbose <- x
    },

    #' @description
    #' Set use_foreach flag
    #' @param x Boolean.
    set_use_foreach = function(x) {
      private$use_foreach <- x
    },

    #' @description
    #' Extracts the data provided via 'add_data' and returns it as a named list.
    #' @return
    #' Named list, where most elements have been added via `add_data`.
    #'
    #' One extra named element is called 'hash'. 'hash' contains the data hashes of particular datasets/variables, as calculated using the 'spookyhash' algorithm via digest::digest.
    #' 'hash' contains two named elements:
    #' - current (the hash of the entire named list)
    #' - current_elements (the hash of the named elements within the named list)
    #' @examples
    #' p <- plnr::Plan$new()
    #' p$add_data("covid_data", fn_name = "plnr::example_data_fn_nor_covid19_cases_by_time_location")
    #' p$get_data()
    get_data = function() {
      retval <- list()
      for (i in seq_along(private$data)) {
        x <- private$data[[i]]
        if (!is.null(x$fn)) {
          retval[[x$name]] <- x$fn()
        }
        if (!is.null(x$fn_name)) {
          retval[[x$name]] <- do.call(get_anything(x$fn_name), list())
        }
        if (!is.null(x$direct)) {
          retval[[x$name]] <- x$direct
        }
      }
      # trying to pull out what happens in sc/sykdomspulsen core
      if (length(retval) == 1) {
        if ("data__________go_up_one_level" %in% names(retval)) {
          # make sure it's a list
          if(inherits(retval$data__________go_up_one_level, "list")){
            # check that if it has content, it is named
            if(!is.null(names(retval$data__________go_up_one_level)) | length(retval$data__________go_up_one_level)==0){
              # this is what happens in sc/sykdomspulsen core
              retval <- retval$data__________go_up_one_level
            } else {
              stop("you are not passing a named list as the return from the data function")
            }
          } else {
            stop("you are not passing a named list as the return from the data function")
          }
        }
      }
      hash <- list()
      hash$current <- digest::digest(retval, algo = "spookyhash")
      hash$current_elements <- list()
      for(i in names(retval)){
        hash$current_elements[[i]] <- digest::digest(retval[[i]], algo = "spookyhash")
      }
      retval$hash <- hash
      return(retval)
    },

    #' @description
    #' Extracts an analysis from the plan.
    #' @param index_analysis Either an integer (1:length(analyses)) or a character string representing the name of the analysis.
    #' @return
    #' An analysis.
    #' @examples
    #' p <- plnr::Plan$new()
    #' p$add_data("covid_data", fn_name = "plnr::example_data_fn_nor_covid19_cases_by_time_location")
    #' batch_argset_list <- list(
    #'   list(name = "analysis_1", var_1 = 1, var_2 = "i"),
    #'   list(name = "analysis_2", var_1 = 2, var_2 = "j"),
    #'   list(name = "analysis_3", var_1 = 3, var_2 = "k")
    #' )
    #' p$add_analysis_from_list(
    #'   fn_name = "plnr::example_action_fn",
    #'   l = batch_argset_list
    #' )
    #' p$get_analysis("analysis_1")
    get_analysis = function(index_analysis) {
      p <- analyses[[index_analysis]]
      p[["argset"]]$index_analysis <- index_analysis
      return(p)
    },

    #' @description
    #' Extracts an argset from the plan.
    #' @param index_analysis Either an integer (1:length(analyses)) or a character string representing the name of the analysis.
    #' @return
    #' An argset
    #' @examples
    #' p <- plnr::Plan$new()
    #' p$add_data("covid_data", fn_name = "plnr::example_data_fn_nor_covid19_cases_by_time_location")
    #' batch_argset_list <- list(
    #'   list(name = "analysis_1", var_1 = 1, var_2 = "i"),
    #'   list(name = "analysis_2", var_1 = 2, var_2 = "j"),
    #'   list(name = "analysis_3", var_1 = 3, var_2 = "k")
    #' )
    #' p$add_analysis_from_list(
    #'   fn_name = "plnr::example_action_fn",
    #'   l = batch_argset_list
    #' )
    #' p$get_argset("analysis_1")
    get_argset = function(index_analysis) {
      p <- analyses[[index_analysis]][["argset"]]
      return(p)
    },

    #' @description
    #' Gets all argsets and presents them as a data.table.
    #' @return
    #' Data.table that contains all the argsets within a plan.
    #' @examples
    #' p <- plnr::Plan$new()
    #' p$add_data("covid_data", fn_name = "plnr::example_data_fn_nor_covid19_cases_by_time_location")
    #' batch_argset_list <- list(
    #'   list(name = "analysis_1", var_1 = 1, var_2 = "i"),
    #'   list(name = "analysis_2", var_1 = 2, var_2 = "j"),
    #'   list(name = "analysis_3", var_1 = 3, var_2 = "k")
    #' )
    #' p$add_analysis_from_list(
    #'   fn_name = "plnr::example_action_fn",
    #'   l = batch_argset_list
    #' )
    #' p$get_argsets_as_dt()
    get_argsets_as_dt = function(){
      retval <- lapply(analyses, function(x) {
        if(identical(x$argset,list())){
          return(data.frame(index_analysis=1))
        } else {
          return(data.frame(t(x$argset)))
        }
      })
      names(retval) <- NULL
      retval <- rbindlist(retval, use.names = T, fill=TRUE)
      retval[, name_analysis := names(analyses)]
      retval[, index_analysis := 1:.N]

      setcolorder(retval, c("name_analysis", "index_analysis"))
      data.table::shouldPrint(retval)

      return(retval)
    },

    #' @description
    #' Run one analysis (data is provided by user).
    #' @param index_analysis Either an integer (1:length(analyses)) or a character string representing the name of the analysis.
    #' @param data Named list (generally obtained from p$get_data()).
    #' @param ... Not used.
    #' @return
    #' Returned value from the action function.
    #' @examples
    #' p <- plnr::Plan$new()
    #' p$add_data("covid_data", fn_name = "plnr::example_data_fn_nor_covid19_cases_by_time_location")
    #' batch_argset_list <- list(
    #'   list(name = "analysis_1", var_1 = 1, var_2 = "i"),
    #'   list(name = "analysis_2", var_1 = 2, var_2 = "j"),
    #'   list(name = "analysis_3", var_1 = 3, var_2 = "k")
    #' )
    #' p$add_analysis_from_list(
    #'   fn_name = "plnr::example_action_fn",
    #'   l = batch_argset_list
    #' )
    #' data <- p$get_data()
    #' p$run_one_with_data("analysis_1", data)
    run_one_with_data = function(index_analysis, data, ...) {
      p <- get_analysis(index_analysis)

      if(is.null(p[["fn"]]) & is.null(p[["fn_name"]])){
        stop("Both fn and fn_name are NULL")
      } else if (!is.null(p[["fn"]]) & is.null(p[["fn_name"]])) {
        # use fn
        num_args <- length(formals(p[["fn"]]))
      } else if (is.null(p[["fn"]]) & !is.null(p[["fn_name"]])) {
        # use fn_name
        num_args <- length(formals(get_anything(p[["fn_name"]])))
      }

      args <- list()
      args[["data"]] <- data
      args[["argset"]] <- p[["argset"]]

      if (num_args < 2) {
        stop("fn must have at least two arguments")
      } else if (num_args == 2) {
        # dont do anything
      } else {
        dots <- list(...)
        for (i in seq_along(dots)) {
          n <- names(dots)[i]
          args[[n]] <- dots[[i]]
        }
      }

      # actually run it
      if (!is.null(p[["fn"]]) & is.null(p[["fn_name"]])) {
        # use fn
        retval <- p$fn(
          data = data,
          p[["argset"]],
          ...
        )
      } else if (is.null(p[["fn"]]) & !is.null(p[["fn_name"]])) {
        # use fn_name
        retval <- do.call(
          what = get_anything(p$fn_name),
          args = args
        )
      }

      return(retval)
    },

    #' @description
    #' Run one analysis (data is obtained automatically from self$get_data()).
    #' @param index_analysis Either an integer (1:length(analyses)) or a character string representing the name of the analysis.
    #' @param ... Not used.
    #' @return
    #' Returned value from the action function.
    #' @examples
    #' p <- plnr::Plan$new()
    #' p$add_data("covid_data", fn_name = "plnr::example_data_fn_nor_covid19_cases_by_time_location")
    #' batch_argset_list <- list(
    #'   list(name = "analysis_1", var_1 = 1, var_2 = "i"),
    #'   list(name = "analysis_2", var_1 = 2, var_2 = "j"),
    #'   list(name = "analysis_3", var_1 = 3, var_2 = "k")
    #' )
    #' p$add_analysis_from_list(
    #'   fn_name = "plnr::example_action_fn",
    #'   l = batch_argset_list
    #' )
    #' p$run_one("analysis_1")
    run_one = function(index_analysis, ...) {
      data <- get_data()
      run_one_with_data(index_analysis = index_analysis, data = data, ...)
    },

    #' @description
    #' Run all analyses (data is provided by user).
    #' @param data Named list (generally obtained from p$get_data()).
    #' @param ... Not used.
    #' @return
    #' List where each element contains the returned value from the action function.
    #' @examples
    #' p <- plnr::Plan$new()
    #' p$add_data("covid_data", fn_name = "plnr::example_data_fn_nor_covid19_cases_by_time_location")
    #' batch_argset_list <- list(
    #'   list(name = "analysis_1", var_1 = 1, var_2 = "i"),
    #'   list(name = "analysis_2", var_1 = 2, var_2 = "j"),
    #'   list(name = "analysis_3", var_1 = 3, var_2 = "k")
    #' )
    #' p$add_analysis_from_list(
    #'   fn_name = "plnr::example_action_fn",
    #'   l = batch_argset_list
    #' )
    #' data <- p$get_data()
    #' p$run_all_with_data(data)
    run_all_with_data = function(data, ...) {
      # try to deparse important arguments
      dots <- list(...)
      if (".plnr.options" %in% names(dots)) {
        chunk_size <- dots[["chunk_size"]]
      } else {
        chunk_size <- 1
      }

      retval <- vector("list", length = self$x_length())
      if (!private$use_foreach_decision()) {
        # running not in parallel
        if (private$verbose & is.null(private$pb_progress) & is.null(private$pb_progressor)) {
          private$pb_progress <- progress::progress_bar$new(
            format = paste0("[:bar] :current/:total (:percent) in :elapsedfull, eta: :eta", ifelse(interactive(), "", "\n")),
            clear = FALSE,
            total = self$x_length()
          )
          private$pb_progress$tick(0)
          on.exit(private$pb_progress <- NULL)
        }

        for (i in self$x_seq_along()) {
          if (private$verbose & !is.null(private$pb_progress)) private$pb_progress$tick()
          if (private$verbose & !is.null(private$pb_progressor)) {
            if (interactive()) {
              private$pb_progressor()
            } else {
              private$pb_progressor()
            }
          }
          retval[[i]] <- run_one_with_data(index_analysis = i, data = data, ...)
          gc(FALSE)
        }
      } else {
        # running in parallel
        if (private$verbose & is.null(private$pb_progress) & is.null(private$pb_progressor)) {
          progressr::handlers(progressr::handler_progress(
            format = "[:bar] :current/:total (:percent) in :elapsedfull, eta: :eta\n",
            clear = FALSE
          ))
          private$pb_progressor <- progressr::progressor(steps = self$x_length())
          on.exit(private$pb_progressor <- NULL)
        }

        retval <- foreach(i = self$x_seq_along(), .options.future = list(chunk.size = chunk_size)) %dopar% {
          if (private$verbose & !is.null(private$pb_progress)) private$pb_progress$tick()
          if (private$verbose & !is.null(private$pb_progressor)) {
            if (interactive()) {
              private$pb_progressor()
            } else {
              private$pb_progressor()
            }
          }
          run_one_with_data(index_analysis = i, data = data, ...)
          gc(FALSE)
        }
      }

      invisible(retval)
    },

    #' @description
    #' Run all analyses (data is obtained automatically from self$get_data()).
    #' @param ... Not used.
    #' @return
    #' List where each element contains the returned value from the action function.
    #' @examples
    #' p <- plnr::Plan$new()
    #' p$add_data("covid_data", fn_name = "plnr::example_data_fn_nor_covid19_cases_by_time_location")
    #' batch_argset_list <- list(
    #'   list(name = "analysis_1", var_1 = 1, var_2 = "i"),
    #'   list(name = "analysis_2", var_1 = 2, var_2 = "j"),
    #'   list(name = "analysis_3", var_1 = 3, var_2 = "k")
    #' )
    #' p$add_analysis_from_list(
    #'   fn_name = "plnr::example_action_fn",
    #'   l = batch_argset_list
    #' )
    #' p$run_all()
    run_all = function(...) {
      data <- get_data()
      run_all_with_data(data = data, ...)
    },

    #' @description
    #' Run all analyses with a progress bar (data is obtained automatically from self$get_data()).
    #' @param ... Not used.
    #' @return
    #' List where each element contains the returned value from the action function.
    #' @examples
    #' p <- plnr::Plan$new()
    #' p$add_data("covid_data", fn_name = "plnr::example_data_fn_nor_covid19_cases_by_time_location")
    #' batch_argset_list <- list(
    #'   list(name = "analysis_1", var_1 = 1, var_2 = "i"),
    #'   list(name = "analysis_2", var_1 = 2, var_2 = "j"),
    #'   list(name = "analysis_3", var_1 = 3, var_2 = "k")
    #' )
    #' p$add_analysis_from_list(
    #'   fn_name = "plnr::example_action_fn",
    #'   l = batch_argset_list
    #' )
    #' p$run_all_progress()
    run_all_progress = function(...) {
      progressr::with_progress(
        {
          run_all(...)
        },
        delay_stdout = F
      )
    },

    #' @description
    #' Run all analyses in parallel (data is obtained automatically from self$get_data()).
    #'
    #' This function only works on linux computers and uses `pbmcapply` as the parallel backend.
    #' @param mc.cores Number of cores to be used.
    #' @param ... Not used.
    #' @return
    #' List where each element contains the returned value from the action function.
    #' @importFrom pbmcapply pbmclapply
    run_all_parallel = function(mc.cores = getOption("mc.cores", 2L), ...){
      data <- self$get_data()
      raw <- pbmcapply::pbmclapply(
        self$x_seq_along(),
        function(x){
          options(mc.cores = 1)
          catch_result <- tryCatch(
            {
              return(self$run_one_with_data(index_analysis = x, data = data, ...))
            },
            error = function(e) {
              system(sprintf('echo "\n%s\n"', paste0("Error in index ", x, ":\n\u2193\u2193\u2193\u2193\u2193\u2193\u2193\u2193\n", e$message, "\n********", collapse="")))
              stop()
            }
          )
        },
        ignore.interactive = TRUE,
        mc.cores = mc.cores,
        mc.style = "ETA",
        mc.substyle = 2
      )
    }
  ),
  private = list(
    verbose = FALSE,
    use_foreach = FALSE,
    pb_progress = NULL,
    pb_progressor = NULL,

    data = list(),

    use_foreach_decision = function() {
      if (!is.null(private$use_foreach)) {
        return(private$use_foreach)
      } else {
        if (foreach::getDoParWorkers() == 1 | !requireNamespace("progressr", quietly = TRUE)) {
          return(FALSE)
        } else {
          return(TRUE)
        }
      }
    }
  )
)


