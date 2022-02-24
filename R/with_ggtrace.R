#' Generic workflow function which localizes a ggtrace call to a single ggplot object
#'
#' `with_ggtrace()` provides a functional interface to `ggtrace()`. It takes a ggplot object
#'  and parameters passed to `ggtrace()` and returns the immediate tracedump and/or graphical
#'  output without side effects.
#'
#' @param x A ggplot object whose evaluation triggers the trace as specified by the `...`
#' @inheritParams get_method
#' @inheritDotParams ggtrace
#' @param out Whether the function should return the output of triggered traces
#'   ("tracedump"), or the resulting graphical object from evaluating the ggplot ("gtable"),
#'   or "both", which returns the tracedump but also renders the resulting plot as a
#'   side effect. Partial matching is supported, so these options could also be specified as
#'   "t", "g", or "b". Defaults to "tracedump".
#'
#' @note To trigger evaluation of `x`, the function `ggeval_silent(x)` is called internally.
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
#'
#' # An example with `out = "gtable"` (or `"g"`)
#' grid_plot <- ggplot(mtcars, aes(mpg, hp)) +
#'   geom_point() +
#'   facet_grid(am ~ cyl)
#' grid_plot
#'
#' outline <- grid::rectGrob(
#'   x = 0.5, y = 0.5, width = 1, height = 1,
#'   gp = grid::gpar(col = "red", lwd = 5, fill = NA)
#' )
#'
#' with_ggtrace(
#'   x = grid_plot,
#'   method = Layout$render,
#'   trace_steps = 5,
#'   trace_exprs = rlang::expr({
#'     panels[c(3, 5)] <- lapply(panels[c(3, 5)], function(panel) {
#'       gTree(children = gList(panel, !!outline))
#'     })
#'   }),
#'   out = "gtable" # or "g"
#' )
#'
#'
#' # With `once = FALSE` for persistent tracing (still cleaned up after)
#' lm_plot <- ggplot(mpg, aes(displ, hwy, color = drv)) +
#'   geom_point() +
#'   geom_smooth(method = "lm")
#' lm_plot
#'
#' with_ggtrace(
#'   x = lm_plot,
#'   method = StatSmooth$compute_group,
#'   trace_steps = c(1, 11),
#'   trace_exprs = list(
#'     group = quote(data$group[1]),
#'     coef = quote(model$coef)
#'   )
#' )
#'
#' with_ggtrace(
#'   x = lm_plot,
#'   method = StatSmooth$compute_group,
#'   trace_steps = 1,
#'   trace_exprs = quote(method <- c("loess", "lm", "loess")[data$group[1]]),
#'   out = "g" # or "gtable"
#' )
#'
#'
#' # `with_ggtrace()` is useful for making calculations that are parasitic
#' on a plot's execution environment.
#'
#' library(grid)
#'
#' square_plot <- ggplot(mtcars, aes(mpg, hp, color = factor(cyl))) +
#'   geom_point() +
#'   theme(aspect.ratio = 1)
#' square_plot
#'
#' rotation_vp <- viewport(width = .7, height = .7, angle = 45)
#'
#' parasitic_evaluations <- with_ggtrace(
#'   x = square_plot,
#'   method = ggplot2:::ggplot_gtable.ggplot_built,
#'   trace_steps = c(9, 13, -1),
#'   trace_exprs = rlang::exprs(
#'     plot_tbl = .plot_table <- editGrob(plot_table, vp = !!rotation_vp),
#'     legend   = .legend <- editGrob(legend_box, vp = viewport(x = 0.15, y = 0.8)),
#'     modified = gTree(children = gList(.plot_table, .legend))
#'   )
#' )
#'
#' grid.newpage()
#' grid.draw(parasitic_evaluations$modified)
#'
with_ggtrace <- function(x, method, ..., out = c("tracedump", "gtable", "both")) {

  method_quo <- rlang::enquo(method)
  if (rlang::is_quosure(method)) {
    method_quo <- method
  }

  if (!inherits(x, "ggplot")) { rlang::abort("`x` must be a ggplot object") }
  suppressMessages({
    prev_silent_opt <- getOption("ggtrace.suppressMessages")
    options("ggtrace.suppressMessages" = TRUE)
    prev_last <- last_ggtrace()
    prev_global <- suppressMessages(global_ggtrace())
    prev_global_state <- global_ggtrace_state()
    global_ggtrace_on()
    clear_global_ggtrace()

    ggtrace(method = method_quo, ...)
    fig <- ggeval_silent(x)
    class(fig) <- c("ggtrace_highjacked", class(fig))
    gguntrace(method_quo)

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
    match.arg(out),
    "tracedump" = dump,
    "gtable"    = fig,
    "both" = {
      ggdraw_silent(fig)
      dump
    }
  )
}
