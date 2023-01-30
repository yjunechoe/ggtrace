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

  inspect_expr <- call(
    paste0("ggtrace_inspect_", step[1]),
    x_expr,
    rlang::parse_expr(step[2])
  )

  if (cond != 1L) inspect_expr$cond <- cond
  if (!isFALSE(error)) inspect_expr$error <- error
  if (step[1] == "args") inspect_expr <- call("$", inspect_expr, quote(data))

  out <- eval.parent(inspect_expr, 2)

  inspect_expr_fmt <- rlang::expr_deparse(inspect_expr, width = Inf)
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

