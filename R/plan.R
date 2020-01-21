#' plan class description
#'
#' An argset is:
#' - a set of arguments
#'
#' An analysis is:
#' - one argset
#' - one function
#'
#' A plan is:
#' - one data pull
#' - a list of analyses
#'
#' @import data.table
#' @import R6
#' @import foreach
#' @export
#' @exportClass Plan
Plan <- R6::R6Class(
  "Plan",
  portable = FALSE,
  cloneable = TRUE,
  public = list(
    data = list(),
    analyses = list(),
    argset_name = "argset",
    verbose = FALSE,
    p = NULL,
    initialize = function(argset_name = "argset", verbose = interactive()) {
      argset_name <<- argset_name
      verbose <<- verbose
    },
    add_data = function(name, fn = NULL, direct = NULL) {
      data[[length(data) + 1]] <<- list(
        fn = fn,
        direct = direct,
        name = name
      )
    },
    add_argset = function(name = uuid::UUIDgenerate(), ...) {
      if (is.null(analyses[[name]])) analyses[[name]] <- list()

      dots <- list(...)
      analyses[[name]][[argset_name]] <<- dots
    },
    add_argset_from_df = function(df) {
      df <- as.data.frame(df)
      for (i in 1:nrow(df)) {
        argset <- df[i, ]
        do.call(add_argset, argset)
      }
    },
    add_analysis = function(name = uuid::UUIDgenerate(), fn = NULL, ...) {
      if (is.null(analyses[[name]])) analyses[[name]] <- list()

      dots <- list(...)
      analyses[[name]] <<- list(fn = fn)
      analyses[[name]][[argset_name]] <<- dots
    },
    add_analysis_from_df = function(fn = NULL, df) {
      df <- as.data.frame(df)
      for (i in 1:nrow(df)) {
        argset <- df[i, ]
        argset$fn <- fn
        do.call(add_analysis, argset)
      }
    },
    apply_analysis_fn_to_all = function(fn) {
      for (i in seq_along(analyses)) {
        analyses[[i]]$fn <<- fn
      }
    },
    len = function() {
      length(analyses)
    },
    x_seq_along = function() {
      base::seq_along(analyses)
    },
    set_progress = function(p) {
      p <<- p
    },
    get_data = function() {
      retval <- list()
      for (i in seq_along(data)) {
        x <- data[[i]]
        if (!is.null(x$fn)) {
          retval[[x$name]] <- x$fn()
        }
        if (!is.null(x$direct)) {
          retval[[x$name]] <- x$direct
        }
      }
      return(retval)
    },
    get_analysis = function(index_analysis) {
      p <- analyses[[index_analysis]]
      p[[argset_name]]$index_analysis <- index_analysis
      return(p)
    },
    get_argset = function(index_analysis) {
      p <- analyses[[index_analysis]][[argset_name]]
      return(p)
    },
    run_one_with_data = function(index_analysis, data, ...) {
      p <- get_analysis(index_analysis)
      if (length(formals(p$fn)) < 2) {
        stop("fn must have at least two arguments")
      } else if (length(formals(p$fn)) == 2) {
        p$fn(
          data = data,
          p[[argset_name]]
        )
      } else {
        p$fn(
          data = data,
          p[[argset_name]],
          ...
        )
      }
    },
    run_one = function(index_analysis, ...) {
      data <- get_data()
      run_one_with_data(index_analysis = index_analysis, data = data, ...)
    },
    run_all = function(...) {
      # try to deparse important arguments
      dots <- list(...)
      if (".plnr.options" %in% names(dots)) {
        chunk_size <- dots[["chunk_size"]]
      } else {
        chunk_size <- 1
      }
      data <- get_data()
      if (verbose & is.null(p)) {
        progressr::handlers(progressr::progress_handler(
          format = "[:bar] :current/:total (:percent) in :elapsedfull, eta: :eta",
          clear = FALSE
        ))
        p <<- progressr::progressor(along = x_seq_along())
        on.exit(p <<- NULL)
      }

      if(foreach::getDoParWorkers()==1){
        # running not in parallel
        print("NOT PARALLEL")
        for(i in x_seq_along()){
          if (verbose & !is.null(p)) p()
          run_one_with_data(index_analysis = i, data = data, ...)
        }
      } else {
        # running in parallel
        y <- foreach(i = x_seq_along(), .options.future = list(chunk.size = chunk_size), .errorhandling="stop") %dopar% {
          if (verbose & !is.null(p)) p()
          run_one_with_data(index_analysis = i, data = data, ...)
        }
      }
    },
    run_all_progress = function(...) {
      progressr::with_progress({
        run_all(...)
      })
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
#     pb <- progressr::progressor(along = plan$x_seq_along())
#     data <- plan$get_data()
#
#     future.apply::future_lapply(plan$x_seq_along(), function(x) {
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
