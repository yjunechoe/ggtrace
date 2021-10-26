.ggtrace_store <- function() {
  .last_ggtrace <- NULL
  .global_ggtrace <- NULL
  .global_ggtrace_state <- TRUE
  list(
    get_last = function() .last_ggtrace,
    set_last = function(value) .last_ggtrace <<- value,
    get_global = function() {
      if(!.global_ggtrace_state) {
        message("Global collection of tracedumps are turned off.\n",
                "To turn it back on, call `global_ggtrace_on()`")
      }
      .global_ggtrace
    },
    set_global = function(value) .global_ggtrace <<- value,
    add_global = function(value) if (.global_ggtrace_state) { .global_ggtrace <<- c(.global_ggtrace, value) },
    get_state = function() .global_ggtrace_state,
    set_state = function(value) .global_ggtrace_state <<- value
  )
}
.ggtrace_storage <- .ggtrace_store()

set_last_ggtrace <- function(value) .ggtrace_storage$set_last(value)
set_global_ggtrace <- function(value) .ggtrace_storage$set_global(value)
add_global_ggtrace <- function(value) .ggtrace_storage$add_global(value)
get_global_state <- function() .ggtrace_storage$get_state()
set_global_state <- function(value) .ggtrace_storage$set_state(value)

#' Retrieve the trace dump created by `ggtrace()`
#'
#' @details
#'
#'   - `last_ggtrace()` retrieves the last trace dump created by `ggtrace()` - i.e., from the last time
#'   the trace has been triggered.
#'
#'   - `global_ggtrace()` is a list of trace dumps collected across multiple traces, and is recommended
#'   for use with `ggtrace(once = FALSE)` when you expect a trace to be independently triggered multiple
#'   times (for example, when you are tracing a (compute/draw)_group method and there are multiple groups, or
#'   when the plot has multiple layers which all call the method being traced).
#'
#'   When a trace dump is pushed to `global_ggtrace()` upon exiting a trace, it gets named after the ggproto method
#'   and a hex code identifying the method's runtime environment, e.g. `"Stat$compute_layer-00000267437FD3D8"`.
#'
#'   - `clear_global_ggtrace()` sets the value of `global_ggtrace()` to `NULL` and returns it.
#'
#' @seealso [ggtrace()], [gguntrace()]
#'
#' @return A list
#' @export
#'
#' @keywords internal
#' @examples
#' library(ggplot2)
#'
#' # Inspect last tracedump
#'
#' ggtrace(StatSmooth$compute_group, trace_steps = -1, trace_exprs = quote(head(prediction)))
#'
#' ggplot(mtcars, aes(mpg, hp)) + geom_point() + geom_smooth(method = 'lm')
#'
#' last_ggtrace()
#'
#' ggtrace(
#'   StatSmooth$compute_group,
#'   trace_steps = -1,
#'   trace_exprs = quote(prediction),
#'   verbose = FALSE
#' )
#'
#' ggplot(mtcars, aes(mpg, hp)) + geom_point() + geom_smooth(method = 'lm')
#'
#' head(last_ggtrace()[[1]])
#'
#'
#' # Inspect an accumulation of trace dumps
#'
#' clear_global_ggtrace()
#' ggtrace(
#'   GeomBoxplot$draw_group,
#'   trace_steps = -1,
#'   once = FALSE,
#'   verbose = FALSE
#' )
#'
#' ggplot(mpg, aes(class, hwy)) + geom_boxplot()
#'
#' gguntrace(GeomBoxplot$draw_group)
#'
#' boxplot_group_tracedump <- global_ggtrace()
#'
#' length(boxplot_group_tracedump)
#'
#' boxplot_group_tracedump <- unlist(
#'   boxplot_group_tracedump,
#'   recursive = FALSE,
#'   use.names = FALSE
#' )
#'
#' patchwork::wrap_plots(boxplot_group_tracedump, nrow = 1)
#'
#' clear_global_ggtrace()
#'
last_ggtrace <- function() .ggtrace_storage$get_last()

#' @export
#' @rdname last_ggtrace
clear_last_ggtrace <- function() {
  set_last_ggtrace(NULL)
  last_ggtrace()
}

#' @export
#' @rdname last_ggtrace
global_ggtrace <- function() .ggtrace_storage$get_global()

#' @export
#' @rdname last_ggtrace
clear_global_ggtrace <- function() {
  set_global_ggtrace(NULL)
  global_ggtrace()
}

#' @param state If missing, returns the current state of global tracing.
#'   Global tracing is `TRUE` by default, meaning that all tracedumps are
#'   stored and available for inspection with `global_ggtrace()`.
#'
#'   You can disable this by setting this argument to `TRUE`
#' @export
#' @rdname last_ggtrace
global_ggtrace_state <- function(state) {
  if (!rlang::is_missing(state) && is.logical(state)) {
    message("Global tracing turned ", if (state) "on" else "off", ".")
    set_global_state(state)
  }
  get_global_state()
}
