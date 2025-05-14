#' Inspect snapshots of sub-layer data
#'
#' `layer_before_stat()`, `layer_after_stat()`, `layer_before_geom()`, and
#' `layer_after_scale()` are convenience functions that return a snapshot of
#' a layer's data in the internals. `layer_is()` is a helper function used by
#' these.
#'
#' @name sublayer-data
#'
#' @param plot A ggplot object. If missing, defaults to `ggplot2::last_plot()`.
#' @param i Index of the layer to inspect. Defaults to `1L`.
#' @param ... Unused.
#' @param error If `TRUE`, returns the layer data early if available before the point of error.
#' @param verbose If `TRUE`, prints the corresponding ggtrace code and re-prints evaluation errors.
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
#' layer_after_scale(p2a, verbose = FALSE)$fill
#' layer_after_scale(p2b, verbose = FALSE)$fill
#' alpha( layer_after_scale(p2a, verbose = FALSE)$fill, .6 )
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
sublayer_data <- function(x, cond = 1L,
                          step = c("before_stat", "after_stat", "before_geom", "after_scale"),
                          ...,
                          error = TRUE,
                          verbose = TRUE,
                          .call = rlang::caller_env()) {

  rlang::check_dots_empty(call = .call)

  step <- .sublayer_stages[[match.arg(step)]]

  ns_ggplot2 <- resolve_ns("ggplot2")
  ns_ggtrace <- resolve_ns("ggtrace")

  if (rlang::is_missing(x)) {
    x_expr <- rlang::call2("last_plot", .ns = ns_ggplot2)
  } else {
    x_expr <- x
  }

  plot <- rlang::eval_bare(x_expr, rlang::caller_env(2))
  if (!inherits(plot, "ggplot")) {
    cli::cli_abort(
      "{.arg plot} must be a {.cls ggplot}, not {.obj_type_friendly {plot}}.",
      call = .call
    )
  }

  inspect_expr <- rlang::call2(
    paste0("inspect_", step[1]),
    x_expr,
    rlang::parse_expr(step[2]),
    .ns = ns_ggtrace
  )

  inspect_expr[[4]] <- rlang::call2("layer_is", cond, .ns = ns_ggtrace)
  if (!isFALSE(error)) inspect_expr$error <- error
  if (step[1] == "args") inspect_expr <- call("$", inspect_expr, quote(data))

  if (!verbose) rethrow_error <- options(ggtrace.rethrow_error = FALSE)
  out <- eval.parent(inspect_expr, 2)
  if (!verbose) options(rethrow_error)

  inspect_expr_fmt <- rlang::expr_deparse(inspect_expr, width = Inf)
  if (verbose) {
    cli::cli_alert_success("Ran {.code {inspect_expr_fmt}}")
  }

  if (rlang::is_installed("tibble")) {
    out <- asNamespace("tibble")$as_tibble(out)
  }

  out

}

#' @rdname sublayer-data
#' @export
layer_before_stat <- function(plot, i = 1L, ..., error = FALSE,
                              verbose = rlang::is_interactive()) {
  sublayer_data(x = rlang::enexpr(plot), cond = as.integer(i), step = "before_stat",
                ..., error = error, verbose = verbose)
}

#' @rdname sublayer-data
#' @export
layer_after_stat <- function(plot, i = 1L, ..., error = FALSE,
                             verbose = rlang::is_interactive()) {
  sublayer_data(x = rlang::enexpr(plot), cond = as.integer(i), step = "after_stat",
                ..., error = error, verbose = verbose)
}

#' @rdname sublayer-data
#' @export
layer_before_geom <- function(plot, i = 1L, ..., error = FALSE,
                              verbose = rlang::is_interactive()) {
  sublayer_data(x = rlang::enexpr(plot), cond = as.integer(i), step = "before_geom",
                ..., error = error, verbose = verbose)
}

#' @rdname sublayer-data
#' @export
layer_after_scale <- function(plot, i = 1L, ..., error = FALSE,
                              verbose = rlang::is_interactive()) {
  sublayer_data(x = rlang::enexpr(plot), cond = as.integer(i), step = "after_scale",
                ..., error = error, verbose = verbose)
}

#' @rdname sublayer-data
#' @param x An integer or an expression.
#'   \itemize{
#'     \item \strong{Integer:} A number for the nth layer
#'     \item \strong{Expression:} Evaluate for each call to the method, which
#'          exposes information about the current layer that the method is being
#'          called for. In technical terms, \code{layer_is()} subsets calls to the method
#'          that are downstream of the \code{by_layer()} function in the ggplot internals.
#'   }
#'   If `x` is an expression, the following variables are exposed:
#'   \itemize{
#'     \item \code{i}: Scalar integer representing the nth layer
#'     \item \code{data}: Data argument passed to the ggproto method, if available.
#'     \item \code{layers}: A list, equivalent to \code{<ggplot>$layers}.
#'   }
#' @export
layer_is <- function(x) {
  if (!rlang::is_expression(x)) {
    if (rlang::is_integerish(x)) {
      x <- is.integer(x)
    } else {
      cli::cli_abort(c(
        "`x` must be an integer or an expression, not {.obj_type_friendly {x}}"
      ))
    }
  }
  rlang::call2(".layer_is", x, .ns = resolve_ns("ggtrace"))
}

#' @export
.layer_is <- function(expr) {

  x <- rlang::enexpr(expr)

  contexts <- c("ggplot_build", "ggplot_gtable")
  invalid_trace_msg <- sprintf(
    "Invalid context: must be called from {.fn %s} or {.fn %s}.",
    contexts[1], contexts[2]
  )
  if (!any(sapply(sys.calls(), rlang::is_call, contexts))) {
    cli::cli_abort(invalid_trace_msg)
  }
  by_layer_idx <- which(sapply(sys.calls(), rlang::is_call, "by_layer"))[1]
  if (is.na(by_layer_idx)) {
    # Don't trigger if not downstream of `by_layer()`
    return(FALSE)
  }
  by_layer_env <- rlang::env_clone(sys.frames()[[by_layer_idx]])

  if (is.numeric(x)) {
    n_layers <- length(by_layer_env$layers)
    if (x > n_layers) {
      # Will never trigger if targeting layer outside of number of layer
      return(FALSE)
    }
    x <- rlang::call2("==", quote(i), as.integer(x))
  }

  # `by_layer()` variables
  keep <- c("i", "layers")
  drop <- setdiff(rlang::env_names(by_layer_env), keep)
  rlang::env_unbind(by_layer_env, nms = drop)

  # ggproto method data variable
  # - climb one frame up to `cond <- eval_tidy(...)` call
  by_layer_env$data <- eval.parent(quote(data))

  isTRUE(rlang::eval_bare(x, by_layer_env))

}
