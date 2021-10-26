.ggtrace_store <- function() {
  .last_ggtrace <- NULL
  .global_ggtrace <- NULL
  .global_ggtrace_state <- TRUE
  list(
    get_last = function() .last_ggtrace,
    set_last = function(value) .last_ggtrace <<- value,
    get_global = function() {
      if(!.global_ggtrace_state) {
        message("Global collection of tracedumps is currently turned off. To activate, call `global_ggtrace_state(TRUE)`")
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

# Moved out as functions because they're used internally in ggtrace()
set_last_ggtrace <- function(value) .ggtrace_storage$set_last(value)
add_global_ggtrace <- function(value) .ggtrace_storage$add_global(value)

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
#' global_ggtrace_state()
#' clear_global_ggtrace()
#'
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
#' global_ggtrace()
#'
#' global_ggtrace_state(FALSE)
#' global_ggtrace_state(TRUE)
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
  .ggtrace_storage$set_global(NULL)
  global_ggtrace()
}

#' @param state If missing, returns whether the global tracedump is currently active.
#'   The global tracedump is active by default (`state` is `TRUE`), meaning that
#'   every time a trace created by `ggtrace()` is triggered, its tracedump is added
#'   to the global tracedump storage, which can be inspected with `global_ggtrace()`.
#'
#'   The global tracedump can be turned on/off by setting `state` to `TRUE`/`FALSE`.
#' @return A logical indicating the current state of the global trace dump.
#'   If `state` is provided, changes the state first, then returns the state invisibly.
#' @export
#' @rdname last_ggtrace
global_ggtrace_state <- function(state) {
  if (!rlang::is_missing(state) && is.logical(state)) {
    message("Global tracedump ", if (state) "activated" else "deactivated", ".")
    .ggtrace_storage$set_state(state)
    invisible(.ggtrace_storage$get_state())
  } else {
    .ggtrace_storage$get_state()
  }
}
