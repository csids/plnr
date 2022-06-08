#' An example action_fn for an analysis
#' @param data Named list.
#' @param argset Named list.
#' @examples
#' p <- plnr::Plan$new()
#' p$add_data("covid_data", fn_name = "plnr::example_data_fn_norway_covid19_cases_by_time_location")
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
#' p$run_one("analysis_1")
#' @export
example_action_fn <- function(data, argset){
  print("Data given:")
  print(names(data))
  print("Argset given:")
  print(names(argset))
}

#' A test action_fn for an analysis that returns the value 1
#' @param data Data
#' @param argset argset
#' @export
test_action_fn <- function(data, argset) {
  return(1)
}

uuid_generator <- function(){
  uuid::UUIDgenerate()
}

hash_it <- function(x){
  digest::digest(x)
}


#' R6 Class representing a Plan
#'
#' @description
#' We work within the mental model where we have one (or more) datasets and we want to run multiple analyses on these datasets.
#'
#' By demanding that all analyses use the same data sources we can:
#' - Be efficient with requiring the minimal amount of data-pulling (this only happens once at the start).
#' - Better enforce the concept that data-cleaning and analysis should be completely separate.
#'
#' By demanding that all analysis functions only use two arguments (data and argset) we can:
#' - Reduce mental fatigue by working within the same mental model for each analysis.
#' - Make it easier for analyses to be exchanged with each other and iterated on.
#' - Easily schedule the running of each analysis.
#'
#' By including all of this in one `Plan` class, we can easily maintain a good overview of all the analyses (i.e. outputs) that need to be run.
#'
#' @details
#' An argset is:
#' - a set of arguments
#'
#' An analysis is:
#' - one argset
#' - one (action) function
#'
#' A plan is:
#' - one data pull
#' - a list of analyses
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
    #' @field analyses List of analyses.
    analyses = list(),

    #' @description Create a new Plan instance.
    #' @param verbose Should this plan be verbose?
    #' @param use_foreach ???
    initialize = function(verbose = interactive() | config$force_verbose, use_foreach = FALSE) {
      private$verbose <- verbose
      # null = program decides
      # false = use loop
      # true = use foreach
      private$use_foreach <- use_foreach
    },

    #' @description Add a new data set.
    #' @param name Name of the data set.
    #' @param fn A function that returns the data set.
    #' @param fn_name A character string containing the name of a function that returns the data set.
    #' @param direct A direct data set.
    #' @examples
    #' p <- plnr::Plan$new()
    #' data_fn <- function(){return(plnr::norway_covid19_cases_by_time_location)}
    #' p$add_data("data_1", fn = data_fn)
    #' p$add_data("data_2", fn_name = "plnr::example_data_fn_norway_covid19_cases_by_time_location")
    #' p$add_data("data_3", direct = plnr::norway_covid19_cases_by_time_location)
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

    #' @description Add a new argset.
    #' @param name Name of the (eventual) analysis that this argset will be connected to.
    #' @param ... Named arguments that will comprise the argset.
    #' @examples
    #' p <- plnr::Plan$new()
    #' p$add_argset("argset_1", var_1 = 3, var_b = "hello")
    #' p$add_argset("argset_2", var_1 = 8, var_c = "hello2")
    #' p$get_argsets_as_dt()
    add_argset = function(name = uuid::UUIDgenerate(), ...) {
      if (is.null(analyses[[name]])) analyses[[name]] <- list()

      dots <- list(...)
      analyses[[name]][["argset"]] <<- dots
    },

    #' @description Add a batch of argsets from a data.frame.
    #' @param df A data.frame where each row is a new argset, and each column will be a named element in the argset.
    #' @examples
    #' p <- plnr::Plan$new()
    #' batch_argset_df <- data.frame(name = c("a", "b", "c"), var_1 = c(1, 2, 3), var_2 = c("i", "j", "k"))
    #' p$add_argset_from_df(batch_argset_df)
    #' p$get_argsets_as_dt()
    add_argset_from_df = function(df) {
      df <- as.data.frame(df)
      for (i in 1:nrow(df)) {
        argset <- df[i, ]
        do.call(self$add_argset, argset)
      }
    },

    #' @description Add a batch of argsets from a list.
    #' @param l A list of lists with named elements where each outermost element is a new argset, and each internal named element named element in the argset.
    #' @examples
    #' p <- plnr::Plan$new()
    #' batch_argset_list <- list(
    #'   list(name = "a", var_1 = 1, var_2 = "i"),
    #'   list(name = "b", var_1 = 2, var_2 = "j"),
    #'   list(name = "c", var_1 = 3, var_2 = "k")
    #' )
    #' p$add_argset_from_list(batch_argset_list)
    #' p$get_argsets_as_dt()
    add_argset_from_list = function(l) {
      for (i in seq_along(l)) {
        argset <- l[[i]]
        do.call(self$add_argset, argset)
      }
      # message(glue::glue("Added {length(l)} argsets to the plan"))
    },

    #' @description Add a new analysis.
    #' @param name Name of the analysis.
    #' @param fn Action function.
    #' @param fn_name Action function name.
    #' @param ... Named arguments to be added to the argset.
    #' @examples
    #' p <- plnr::Plan$new()
    #' p$add_data("covid_data", fn_name = "plnr::example_data_fn_norway_covid19_cases_by_time_location")
    #' p$add_analysis(
    #'   name = "analysis_1",
    #'   fn_name = "plnr::example_action_fn"
    #' )
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

    #' @description Add a batch of analyses from a data.frame.
    #' @param fn Action function.
    #' @param fn_name Action function name.
    #' @param df A data.frame where each row is a new argset, and each column will be a named element in the argset.
    #' @examples
    #' p <- plnr::Plan$new()
    #' p$add_data("covid_data", fn_name = "plnr::example_data_fn_norway_covid19_cases_by_time_location")
    #' batch_argset_df <- data.frame(name = c("a", "b", "c"), var_1 = c(1, 2, 3), var_2 = c("i", "j", "k"))
    #' p$add_analysis_from_df(
    #'   fn_name = "plnr::example_action_fn",
    #'   df = batch_argset_df
    #'  )
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
        print(argset)
        do.call(add_analysis, argset)
      }
    },

    #' @description Add a batch of argsets from a list.
    #' @param fn Action function.
    #' @param fn_name Action function name.
    #' @param l A list of lists with named elements where each outermost element is a new argset, and each internal named element named element in the argset.
    #' @examples
    #' p <- plnr::Plan$new()
    #' p$add_data("covid_data", fn_name = "plnr::example_data_fn_norway_covid19_cases_by_time_location")
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
    #' p$run_one("analysis_1")
    add_analysis_from_list = function(fn = NULL, fn_name = NULL, l) {
      stopifnot(is.null(fn) | is.function(fn))
      stopifnot(is.null(fn_name) | is.character(fn_name))

      for (i in seq_along(l)) {
        argset <- l[[i]]
        argset$fn <- fn
        if (!"fn_name" %in% names(df)) argset$fn_name <- fn_name
        # message(i)
        do.call(add_analysis, argset)
      }
      # message(glue::glue("Added {length(l)} analyses to the plan"))
    },

    #' @description Applies an action function to all the argsets
    #' @param fn Action function.
    #' @param fn_name Action function name.
    #' p <- plnr::Plan$new()
    #' p$add_data("covid_data", fn_name = "plnr::example_data_fn_norway_covid19_cases_by_time_location")
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
    #' p$add_data("covid_data", fn_name = "plnr::example_data_fn_norway_covid19_cases_by_time_location")
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
    #' p$add_data("covid_data", fn_name = "plnr::example_data_fn_norway_covid19_cases_by_time_location")
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
    #' p$add_data("covid_data", fn_name = "plnr::example_data_fn_norway_covid19_cases_by_time_location")
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
    #' p$add_data("covid_data", fn_name = "plnr::example_data_fn_norway_covid19_cases_by_time_location")
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
    #' p$add_data("covid_data", fn_name = "plnr::example_data_fn_norway_covid19_cases_by_time_location")
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
    #' p$add_data("covid_data", fn_name = "plnr::example_data_fn_norway_covid19_cases_by_time_location")
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
    #' p$add_data("covid_data", fn_name = "plnr::example_data_fn_norway_covid19_cases_by_time_location")
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
    #' p$add_data("covid_data", fn_name = "plnr::example_data_fn_norway_covid19_cases_by_time_location")
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
    #' p$add_data("covid_data", fn_name = "plnr::example_data_fn_norway_covid19_cases_by_time_location")
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

# #' run_all_parallel
# #' @param plan a
# #' @param cores a
# #' @param future.chunk.size Size of future chunks
# #' @param verbose a
# #' @param multisession a
# #' @export
# run_all_parallel <- function(
#   plan,
#   cores = parallel::detectCores(),
#   future.chunk.size = NULL,
#   verbose = interactive(),
#   multisession = TRUE){
#
#   if(multisession){
#     future::plan(future::multisession, workers = cores, earlySignal = TRUE)
#   } else {
#     future::plan(future.callr::callr, workers = cores, earlySignal = TRUE)
#   }
#   on.exit(future:::ClusterRegistry("stop"))
#
#   progressr::handlers(progressr::progress_handler(
#     format = "[:bar] :current/:total (:percent) in :elapsedfull, eta: :eta",
#     clear = FALSE
#     ))
#
#   y <- progressr::with_progress({
#     pb <- progressr::progressor(along = plan$seq_along())
#     data <- plan$get_data()
#
#     future.apply::future_lapply(plan$seq_along(), function(x) {
#       pb(sprintf("x=%g", x))
#       plan$run_one_with_data(index_arg = x, data = data)
#     }, future.chunk.size = future.chunk.size)
#   })
# }
#'
#' #' Plans
#' #' @import data.table
#' #' @import R6
#' #' @export
#' #' @exportClass Plans
#' Plans <- R6::R6Class(
#'   "Plans",
#'   portable = FALSE,
#'   cloneable = TRUE,
#'   public = list(
#'     list_plan = list(),
#'     initialize = function() {
#'     },
#'     add_plan = function(p) {
#'       list_plan[[length(list_plan) + 1]] <<- Plan$new()
#'
#'       # add data
#'       for (i in seq_along(p$data)) {
#'         list_plan[[length(list_plan)]]$add_data(
#'           fn = p$data[[i]]$fn,
#'           df = p$data[[i]]$df,
#'           name = p$data[[i]]$name
#'         )
#'       }
#'
#'       # add analyses
#'       for (i in seq_along(p$list_arg)) {
#'         arg <- p$list_arg[[i]]$arg
#'         arg$fn <- p$list_arg[[i]]$fn
#'
#'         do.call(list_plan[[length(list_plan)]]$add_analysis, arg)
#'       }
#'     },
#'     add_analysis = function(fn, ...) {
#'       list_arg[[length(list_arg) + 1]] <<- list(
#'         fn = fn,
#'         arg = ...
#'       )
#'     },
#'     len = function(index_plan) {
#'       if (missing(index_plan)) {
#'         return(length(list_plan))
#'       } else {
#'         return(length(list_plan[[index_plan]]))
#'       }
#'     },
#'     x_seq_along = function(index_plan) {
#'       if (missing(index_plan)) {
#'         return(seq_along(list_plan))
#'       } else {
#'         return(seq_along(list_plan[[index_plan]]))
#'       }
#'     },
#'     get_data = function(index_plan) {
#'       list_plan[[index_plan]]$get_data()
#'     },
#'     get_analysis = function(index_plan, index_arg) {
#'       list_plan[[index_plan]]$get_analysis(index_arg)
#'     },
#'     analysis_run = function(data, analysis) {
#'       analysis$fn(
#'         data = data,
#'         arg = analysis$arg
#'       )
#'     }
#'   )
#' )
