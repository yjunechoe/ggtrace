#' Simple evaluation and printing of a ggplot object
#'
#' `ggeval_silent()` evaluates the ggplot object into a `<gtable>` grob and `ggdraw_silent()`
#' renders the grob with `{grid}`. Combined, these two functions simulate `ggplot2:::print.ggplot`
#' without overriding `last_plot()`.
#'
#' @param x A ggplot
#'
#' @return A gtable
#' @keywords internal
ggeval_silent <- function(x) {
  invisible(asNamespace("ggplot2")$ggplotGrob(x))
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
}

resolve_cond <- function(x, multiple = FALSE) {
  ._counter_ <- NULL # bypass notes
  if (is.numeric(x)) {
    if (multiple && length(x) > 1L) {
      x <- rlang::expr(._counter_ %in% !!as.integer(x))
    } else {
      x <- rlang::expr(._counter_ == !!as.integer(x))
    }
  }
  x
}

simulate_plot <- function(x, error) {
  if (error) {
    log <- NULL
    tryCatch(expr = ggeval_silent(x), error = function(e) {
      log <<- e
    })
    if (!is.null(log)) {
      log$trace <- NULL
      msg <- paste0(rlang::cnd_message(log, prefix = TRUE), "\n")
      writeLines(msg)
    }
  } else {
    ggeval_silent(x)
  }
  invisible(NULL)
}

.ggtrace_placeholder <- structure(list(), class = "ggtrace_placeholder")

is.ggtrace_placeholder <- function(x) {
  (length(class(x)) == 1) && (class(x) == "ggtrace_placeholder")
}
