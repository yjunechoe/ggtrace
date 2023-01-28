#' Inspect snapshots of sub-layer data
#'
#' @name sublayer-data
#'
#' @param x A ggplot object. If missing, defaults to `ggplot2::last_plot()`.
#' @param cond Index of the layer to inspect. Defaults to `1L`.
#' @param error For debugging. If `TRUE`, continues inspecting the method until the ggplot errors.
#' @param ... Unused.
#'
#' @return A dataframe
#'
#' @keywords internal
NULL

#' @keywords internal
.sublayer_stages <- list(
  before_stat = c("args", "ggplot2:::Layer$compute_statistic"),
  after_stat = c("return", "ggplot2:::Layer$map_statistic"),
  before_geom = c("args", "ggplot2:::Layer$compute_geom_1"),
  after_scale = c("return", "ggplot2:::Layer$compute_geom_2")
)

#' @keywords internal
sublayer_data <- function(x, cond, error,
                          step = c("before_stat", "after_stat", "before_geom", "after_scale"), ...) {

  step <- .sublayer_stages[[match.arg(step)]]

  if (rlang::quo_is_missing(x)) {
    x_expr <- call("last_plot")
  } else {
    x_expr <- rlang::quo_get_expr(x)
  }

  inspect_expr <- call(
    paste0("ggtrace_inspect_", step[1]),
    x_expr,
    rlang::parse_expr(step[2])
  )

  if (cond != 1L) inspect_expr$cond <- cond
  if (!isFALSE(error)) inspect_expr$error <- error
  if (step[1] == "args") inspect_expr <- call("$", inspect_expr, quote(data))

  out <- eval(inspect_expr, envir = rlang::quo_get_env(x))

  inspect_expr_fmt <- rlang::expr_deparse(inspect_expr, width = Inf)
  cli::cli_alert_success("Executed {.code {inspect_expr_fmt}}")

  if (rlang::is_installed("tibble")) {
    out <- asNamespace("tibble")$as_tibble(out)
  }

  out

}

#' @rdname sublayer-data
layer_before_stat <- function(x, cond = 1L, error = FALSE, ...) {
  sublayer_data(rlang::new_quosure(rlang::enexpr(x), parent.frame()), cond, error, step = "before_stat", ...)
}

#' @rdname sublayer-data
layer_after_stat <- function(x, cond = 1L, error = FALSE, ...) {
  sublayer_data(rlang::new_quosure(rlang::enexpr(x), parent.frame()), cond, error, step = "after_stat", ...)
}

#' @rdname sublayer-data
layer_before_geom <- function(x, cond = 1L, error = FALSE, ...) {
  sublayer_data(rlang::new_quosure(rlang::enexpr(x), parent.frame()), cond, error, step = "before_geom", ...)
}

#' @rdname sublayer-data
layer_after_scale <- function(x, cond = 1L, error = FALSE, ...) {
  sublayer_data(rlang::new_quosure(rlang::enexpr(x), parent.frame()), cond, error, step = "after_scale", ...)
}

