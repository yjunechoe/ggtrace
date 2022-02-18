#' Simple evaluation and printing of a ggplot object
#'
#' `ggeval_silent()` evaluates the ggplot object into a `<gtable>` grob and `ggdraw_silent()`
#' renders the grob with `{grid}`. Combined, these two functions simulate `ggplot2:::print.ggplot`
#' without overriding `last_plot()`.
#'
#' @param x A ggplot
#'
#' @return A gtable
#' @export
ggeval_silent <- function(x) {
  asNamespace("ggplot2")$ggplot_gtable(asNamespace("ggplot2")$ggplot_build(x))
}

#' @export
#' @rdname ggeval_silent
ggdraw_silent <- function(x) {
  asNamespace("grid")$grid.newpage()
  asNamespace("grid")$grid.draw(x)
  invisible(x)
}

#' @export
print.ggtrace_highjacked <- function(x, ...) {
  ggdraw_silent(x)
  invisible(x)
}
