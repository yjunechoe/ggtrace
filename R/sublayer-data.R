#' Inspect snapshots of sub-layer data
#'
#' `layer_before_stat()`, `layer_after_stat()`, `layer_before_geom()`, and `layer_after_scale()`
#' are helper functions that return a snapshot of a layer's data in the internals.
#'
#' @name sublayer-data
#'
#' @param plot A ggplot object. If missing, defaults to `ggplot2::last_plot()`.
#' @param i Index of the layer to inspect. Defaults to `1L`.
#' @param ... Unused.
#' @param error For debugging. If `TRUE`, continues inspecting the method until the ggplot errors.
#'
#' @return A dataframe
#'
#' @keywords internal
#'
#' @examples
#' library(ggplot2)
#' p1 <- ggplot(mpg, aes(displ, class)) +
#'   geom_boxplot(outlier.shape = NA) +
#'   geom_text(
#'     aes(
#'       label = after_stat(xmax),
#'       x = stage(displ, after_stat = xmax)
#'     ),
#'     stat = "boxplot", hjust = -0.5
#'   )
#' p1
#'
#' # Before Stat snapshot of first layer's data
#' layer_before_stat()
#'
#' # After Stat snapshot of first layer's data
#' layer_after_stat()
#'
#' # First and second layer's data are identical for those two stages
#' identical(layer_before_stat(), layer_before_stat(i = 2))
#' identical(layer_after_stat(), layer_after_stat(i = 2))
#'
#' # `after_stat()` mappings add new columns to the second layer's data
#' # by the time the geom receives the data in the Before Geom stage
#' library(dplyr)
#' layer_before_geom(i = 2)
#'
#' # After Scale data reflects `after_scale()` mappings
#' p2 <- ggplot(mpg, aes(as.factor(cyl), hwy, color = as.factor(cyl))) +
#'   theme(legend.position = 0)
#' p2a <- p2 +
#'   geom_boxplot(aes(fill = as.factor(cyl)))
#' p2b <- p2 +
#'   geom_boxplot(aes(fill = after_scale(alpha(color, .6))))
#'
#' library(patchwork)
#' p2a + p2b
#'
#' layer_after_scale(p2a)$fill
#' layer_after_scale(p2b)$fill
#' alpha( layer_after_scale(p2a)$fill, .6 )
#'
NULL

#' @keywords internal
.sublayer_stages <- list(
  before_stat = c("args", "ggplot2:::Layer$compute_statistic"),
  after_stat = c("return", "ggplot2:::Layer$compute_statistic"),
  before_geom = c("args", "ggplot2:::Layer$compute_geom_1"),
  after_scale = c("return", "ggplot2:::Layer$compute_geom_2")
)

#' @keywords internal
sublayer_data <- function(x, cond = 1L, error = TRUE,
                          step = c("before_stat", "after_stat", "before_geom", "after_scale"), ...) {

  step <- .sublayer_stages[[match.arg(step)]]

  if (rlang::is_missing(x)) {
    x_expr <- call("last_plot")
  } else {
    x_expr <- x
  }

  inspect_expr <- rlang::call2(
    paste0("ggtrace_inspect_", step[1]),
    x_expr,
    rlang::parse_expr(step[2]),
    .ns = "ggtrace"
  )

  if (cond != 1L) inspect_expr$cond <- cond
  if (!isFALSE(error)) inspect_expr$error <- error
  if (step[1] == "args") inspect_expr <- call("$", inspect_expr, quote(data))

  out <- eval.parent(inspect_expr, 2)

  inspect_expr_fmt <- rlang::expr_deparse(inspect_expr, width = Inf)
  inspect_expr_fmt <- gsub("^ggtrace::", "", inspect_expr_fmt)
  cli::cli_alert_success("Executed {.code {inspect_expr_fmt}}")

  if (rlang::is_installed("tibble")) {
    out <- asNamespace("tibble")$as_tibble(out)
  }

  out

}

#' @rdname sublayer-data
#' @export
layer_before_stat <- function(plot, i = 1L, ..., error = FALSE) {
  sublayer_data(x = rlang::enexpr(plot), cond = i, error, step = "before_stat", ...)
}

#' @rdname sublayer-data
#' @export
layer_after_stat <- function(plot, i = 1L, ..., error = FALSE) {
  sublayer_data(x = rlang::enexpr(plot), cond = i, error, step = "after_stat", ...)
}

#' @rdname sublayer-data
#' @export
layer_before_geom <- function(plot, i = 1L, ..., error = FALSE) {
  sublayer_data(x = rlang::enexpr(plot), cond = i, error, step = "before_geom", ...)
}

#' @rdname sublayer-data
#' @export
layer_after_scale <- function(plot, i = 1L, ..., error = FALSE) {
  sublayer_data(x = rlang::enexpr(plot), cond = i, error, step = "after_scale", ...)
}

