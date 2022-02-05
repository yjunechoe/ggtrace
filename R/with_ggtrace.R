#' Isolate a ggtrace call for a single object
#'
#' `with_ggtrace()` provides a functional interface to `ggtrace()`. It takes an object
#'  and parameters passed to `ggtrace()` and returns the immediate tracedump without side effects.
#'
#' @param x A ggplot object whose evaluation triggers the trace as specified by the `...`
#' @inheritDotParams ggtrace
#'
#' @note To force evaluation of `x`, `ggeval_silent(x)` is called internally.
#'
#' @seealso [ggtrace()], [ggeval_silent()]
#'
#' @return A list
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Long-form `ggtrace()` method:
#' boxplot_plot <- ggplot(diamonds[1:500,], aes(cut, depth)) + geom_boxplot()
#' ggtrace(
#'  method = StatBoxplot$compute_group,
#'  trace_steps = -1, trace_exprs = quote(~step)
#' )
#' boxplot_plot
#' first_tracedump <- last_ggtrace()
#'
#' # Short-form functional `with_ggtrace()` method:
#' second_tracedump <- with_ggtrace(
#'   x = boxplot_plot,
#'   method = StatBoxplot$compute_group,
#'   trace_steps = -1, trace_exprs = quote(~step)
#' )
#'
#' identical(first_tracedump, second_tracedump)
#'
with_ggtrace <- function(x, ...) {
  if (!is.ggplot(x)) { rlang::abort("`x` must be a ggplot object") }
  suppressMessages({
    prev_silent_opt <- getOption("ggtrace.suppressMessages")
    options("ggtrace.suppressMessages" = TRUE)
    prev_last <- last_ggtrace()
    prev_global <- suppressMessages(global_ggtrace())
    prev_global_state <- global_ggtrace_state()
    global_ggtrace_on()
    clear_global_ggtrace()

    ggtrace(...)
    ggeval_silent(x)
    gguntrace(...)

    out <- global_ggtrace()
    if (length(out) == 1L) { out <- out[[1]] }

    set_last_ggtrace(prev_last)
    global_ggtrace_state(prev_global_state)
    set_global_ggtrace(prev_global)
    options("ggtrace.suppressMessages" = prev_silent_opt)
  })
  out
}


