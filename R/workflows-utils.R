#' Simulate printing of a ggplot object without side effects
#'
#' Calls `ggplot_gtable(ggplot_build(x))`, which executes all the instructions for plotting `x` without
#' the side-effects from `ggplot2:::print.ggplot`, namely:
#'
#' - `last_plot()` is not overriden
#' - `grid.draw()` is not called (= nothing gets rendered/drawn)
#'
#' @param x A ggplot
#'
#' @return A gtable
#' @export
ggeval_silent <- function(x) {
  asNamespace("ggplot2")$ggplot_gtable(asNamespace("ggplot2")$ggplot_build(x))
}

ggdraw_silent <- function(x) {
  asNamespace("grid")$grid.newpage()
  asNamespace("grid")$grid.draw(x)
}

#' @export
print.ggtrace_modified <- function(x, ...) {
  ggdraw_silent(x)
  invisible(x)
}
