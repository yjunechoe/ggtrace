.ggtrace_store <- function() {
  .last_ggtrace <- NULL
  .global_ggtrace <- NULL
  list(
    get_last = function() .last_ggtrace,
    set_last = function(value) .last_ggtrace <<- value,
    get_global = function() .global_ggtrace,
    set_global = function(value) .global_ggtrace <<- value,
    add_global = function(value) .global_ggtrace <<- c(.global_ggtrace, value)
  )
}
.ggtrace_storage <- .ggtrace_store()

set_last_ggtrace <- function(value) .ggtrace_storage$set_last(value)
set_global_ggtrace <- function(value) .ggtrace_storage$set_global(value)
add_global_ggtrace <- function(value) .ggtrace_storage$add_global(value)

#' Retrieve the trace dump created by `ggtrace()`
#'
#' @details `last_ggtrace()` retrieves the last trace dump created by `ggtrace()` - i.e., from the last time
#'   the trace has been triggered. `global_ggtrace()` is a collection of trace dumps tracked across
#'   multiple traces, and is recommended for with `ggtrace(once = FALSE)` where you expect a trace to
#'   be separately triggered multiple times (for example, when you are tracing a compute or draw method
#'   for group and there are multiple groups). `clear_global_ggtrace()` sets the value of the global trace
#'   dump to `NULL`.
#'
#' @seealso [ggtrace()], [gguntrace()]
#'
#' @return A list
#' @export
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # Inspect last trace dump
#'
#' ggbody(StatSmooth$compute_group)
#'
#' ggtrace(StatSmooth$compute_group, trace_steps = 12, trace_exprs = quote(head(prediction)))
#'
#' ggplot(mtcars, aes(mpg, hp)) + geom_point() + geom_smooth(method = 'lm')
#'
#' last_ggtrace()
#'
#' ggtrace(
#'   StatSmooth$compute_group,
#'   trace_steps = 12,
#'   trace_exprs = quote(prediction),
#'   .print = FALSE
#' )
#'
#' ggplot(mtcars, aes(mpg, hp)) + geom_point() + geom_smooth(method = 'lm')
#'
#' last_ggtrace()
#'
#'
#' # Inspect an accumulation of trace dumps
#'
#' clear_global_ggtrace()
#' ggtrace(
#'   GeomBoxplot$draw_group,
#'   trace_steps = -1,
#'   once = FALSE,
#'   .print = FALSE
#' )
#'
#' ggplot(mpg, aes(class, hwy)) + geom_boxplot()
#'
#' gguntrace(GeomBoxplot$draw_group)
#'
#' boxplot_group_tracedump <- global_ggtrace()
#' clear_global_ggtrace()
#'
#' length(boxplot_group_tracedump)
#' boxplot_group_tracedump <- unlist(
#'   boxplot_group_tracedump,
#'   recursive = FALSE,
#'   use.names = FALSE
#' )
#'
#' patchwork::wrap_plots(boxplot_group_tracedump, nrow = 1)
#'
#' }
last_ggtrace <- function() .ggtrace_storage$get_last()

#' @export
#' @rdname last_ggtrace
global_ggtrace <- function() .ggtrace_storage$get_global()

#' @export
#' @rdname last_ggtrace
clear_global_ggtrace <- function() {
  set_global_ggtrace(NULL)
  global_ggtrace()
}
