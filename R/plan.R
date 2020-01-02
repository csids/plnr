#' plan class description
#'
#' @import data.table
#' @import R6
#' @export
#' @exportClass Plan
Plan <- R6::R6Class(
  "Plan",
  portable = FALSE,
  cloneable = TRUE,
  public = list(
    list_data = list(),
    list_analysis = list(),
    name_arg = "arg",
    initialize = function(name_arg = "arg") {
      name_arg <<- name_arg
    },
    data_add = function(fn=NULL, df=NULL, name) {
      list_data[[length(list_data) + 1]] <<- list(
        fn = fn,
        df = df,
        name = name
      )
    },
    analysis_add = function(fn=NULL, name=uuid::UUIDgenerate(), ...) {
      dots <- list(...)
      list_analysis[[name]] <<- list(fn = fn)
      list_analysis[[name]][[name_arg]] <<- dots
    },
    analysis_add_from_df = function(fn=NULL, df){
      df <- as.data.frame(df)
      for(i in 1:nrow(df)){
        arg <- df[i, ]
        arg$fn <- fn
        do.call(analysis_add, arg)
      }
    },
    analysis_fn_apply_to_all = function(fn){
      for(i in seq_along(list_analysis)){
        list_analysis[[i]]$fn <<- fn
      }
    },
    len = function(){
      length(list_analysis)
    },
    x_seq_along = function(){
      base::seq_along(list_analysis)
    },
    data_get = function(){
      data <- list()
      for(i in seq_along(list_data)){
        x <- list_data[[i]]
        if(!is.null(x$fn)){
          data[[x$name]] <- x$fn()
        }
        if(!is.null(x$df)){
          data[[x$name]] <- x$df
        }
      }
      return(data)
    },
    analysis_get = function(index_analysis){
      p <- list_analysis[[index_analysis]]
      p[[name_arg]]$index_analysis <- index_analysis
      return(p)
    },
    run_one_with_data = function(index_analysis, data){
      p <- analysis_get(index_analysis)
      p$fn(
        data = data,
        p[[name_arg]]
      )
    },
    run_one = function(index_analysis){
      data <- data_get()
      run_one_with_data(index_analysis = index_analysis, data = data)
    },
    run_all = function(verbose = interactive()){
      data <- data_get()
      if(verbose) pb <- fhi::txt_progress_bar(max=len())
      for(i in x_seq_along()){
        run_one_with_data(index_analysis = i, data = data)
        if(verbose) utils::setTxtProgressBar(pb, value = i)
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
#     pb <- progressr::progressor(along = plan$x_seq_along())
#     data <- plan$data_get()
#
#     future.apply::future_lapply(plan$x_seq_along(), function(x) {
#       pb(sprintf("x=%g", x))
#       plan$run_one_with_data(index_analysis = x, data = data)
#     }, future.chunk.size = future.chunk.size)
#   })
# }

#' Plans
#' @import data.table
#' @import R6
#' @export
#' @exportClass Plans
Plans <- R6::R6Class(
  "Plans",
  portable = FALSE,
  cloneable = TRUE,
  public = list(
    list_plan = list(),
    initialize = function() {
    },
    plan_add = function(p) {
      list_plan[[length(list_plan) + 1]] <<- Plan$new()

      # add data
      for(i in seq_along(p$list_data)) list_plan[[length(list_plan)]]$data_add(
        fn = p$list_data[[i]]$fn,
        df = p$list_data[[i]]$df,
        name = p$list_data[[i]]$name
      )

      # add analyses
      for(i in seq_along(p$list_analysis)){
        arg <- p$list_analysis[[i]]$arg
        arg$fn <- p$list_analysis[[i]]$fn

        do.call(list_plan[[length(list_plan)]]$analysis_add, arg)
      }
    },
    analysis_add = function(fn, ...) {
      list_analysis[[length(list_analysis) + 1]] <<- list(
        fn = fn,
        arg = ...
      )
    },
    len = function(index_plan){
      if(missing(index_plan)){
        return(length(list_plan))
      } else {
        return(length(list_plan[[index_plan]]))
      }
    },
    x_seq_along = function(index_plan){
      if(missing(index_plan)){
        return(seq_along(list_plan))
      } else {
        return(seq_along(list_plan[[index_plan]]))
      }
    },
    data_get = function(index_plan){
      list_plan[[index_plan]]$data_get()
    },
    analysis_get = function(index_plan, index_analysis){
      list_plan[[index_plan]]$analysis_get(index_analysis)
    },
    analysis_run = function(data, analysis) {
      analysis$fn(
        data = data,
        arg = analysis$arg
      )
    }
  )
)


