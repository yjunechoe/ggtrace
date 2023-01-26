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
  before_stat = "ggplot2:::Layer$compute_statistic",
  after_stat = "ggplot2:::Layer$compute_statistic"
)

#' @rdname sublayer-data
#'
#' @export
layer_before_stat <- function(x, cond = 1L, error = FALSE, ...) {

  if (missing(x)) {
    x <- call("last_plot")
  } else {
    x_expr <- rlang::enexpr(x)
    if (!rlang::is_symbol(x)) x <- x_expr
  }

  inspect_expr <- call(
    "ggtrace_inspect_args",
    x,
    rlang::parse_expr(.sublayer_stages$before_stat)
  )

  if (!missing(cond)) inspect_expr$cond <- cond
  if (!missing(error)) inspect_expr$error <- error

  inspect_expr <- call("$", inspect_expr, quote(data))

  out <- eval.parent(inspect_expr)

  inspect_expr_fmt <- rlang::expr_deparse(inspect_expr, width = Inf)
  cli::cli_alert_success("Executed {.code {inspect_expr_fmt}}")

  if (rlang::is_installed("tibble")) {
    out <- asNamespace("tibble")$as_tibble(out)
  }

  out

}

#' @rdname sublayer-data
#'
#' @export
layer_after_stat <- function(x, cond = 1L, error = FALSE, ...) {

  if (missing(x)) {
    x <- call("last_plot")
  } else {
    x_expr <- rlang::enexpr(x)
    if (!rlang::is_symbol(x)) x <- x_expr
  }

  inspect_expr <- call(
    "ggtrace_inspect_return",
    x,
    rlang::parse_expr(.sublayer_stages$after_stat)
  )

  if (!missing(cond)) inspect_expr$cond <- cond
  if (!missing(error)) inspect_expr$error <- error

  out <- eval.parent(inspect_expr)

  inspect_expr_fmt <- rlang::expr_deparse(inspect_expr, width = Inf)
  cli::cli_alert_success("Executed {.code {inspect_expr_fmt}}")

  if (rlang::is_installed("tibble")) {
    out <- asNamespace("tibble")$as_tibble(out)
  }

  out

}
