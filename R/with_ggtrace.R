#' Apply a ggtrace call to a single ggplot object
#'
#' `with_ggtrace()` provides a functional interface to `ggtrace()`. It takes a ggplot object
#'  and parameters passed to `ggtrace()` and returns the immediate tracedump without side effects.
#'
#'  It is the lower-level function that powers all workflow functions in `{ggtrace}`.
#'
#' @param x A ggplot object whose evaluation triggers the trace as specified by the `...`
#' @inheritDotParams ggtrace
#' @param return_value Whether the function should return the output of triggered traces
#'   ("tracedump"), or the resulting graphical object from evaluating the ggplot ("gtable"),
#'   or "both", which returns the tracedump but also renders the resulting plot as a
#'   side effect. Defaults to "tracedump".
#'
#' @note To force evaluation of `x`, `ggeval_silent(x)` is called internally.
#'
#' @seealso [ggtrace()], [ggeval_silent()]
#'
#' @return A list or gtable object of class `<ggtrace_highjacked>`
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
with_ggtrace <- function(x, ..., return_value = c("tracedump", "gtable", "both")) {
  if (!inherits(x, "ggplot")) { rlang::abort("`x` must be a ggplot object") }
  suppressMessages({
    prev_silent_opt <- getOption("ggtrace.suppressMessages")
    options("ggtrace.suppressMessages" = TRUE)
    prev_last <- last_ggtrace()
    prev_global <- suppressMessages(global_ggtrace())
    prev_global_state <- global_ggtrace_state()
    global_ggtrace_on()
    clear_global_ggtrace()

    ggtrace(...)
    fig <- ggeval_silent(x)
    gguntrace(...)

    dump <- global_ggtrace()
    if (length(dump) == 1L) { dump <- dump[[1]] }

    set_last_ggtrace(prev_last)
    global_ggtrace_state(prev_global_state)
    set_global_ggtrace(prev_global)
    options("ggtrace.suppressMessages" = prev_silent_opt)
  })
  if (is.null(dump)) {
    rlang::abort("Trace was not triggered - did the plot call the method?")
  }

  switch(
    match.arg(return_value),
    "tracedump" = dump,
    "gtable"    = invisible(fig),
    "both" = {
      ggdraw_silent(fig)
      dump
    }
  )
}
