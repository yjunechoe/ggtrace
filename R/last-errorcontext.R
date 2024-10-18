#' Get the internal context of the last (sub-)layer error
#'
#' @description
#' - `last_layer_errorcontext()` returns the error context at the level of the `Layer` ggproto.
#' - `last_sublayer_errorcontext()` (EXPERIMENTAL) returns the error context at the sub-`Layer` level (e.g., `Stat` or `Geom`).
#'
#' @section Scope:
#' These functions can only retrieve information from errors propagating from
#' `Layer` ggproto methods. In non-technical terms, they only work for errors with a
#' **"Error occured in the Nth layer"** message (as of `{ggplot2}` >= 3.4.0).
#'
#' The scope of `last_sublayer_errorcontext()` is narrower, since not all `Layer` methods call
#' a sub-Layer method. This function is intended for developers - in most cases users can
#' get all the information necessary to debug layer code from `last_layer_errorcontext()`
#' (there are only so many ways to break a ggplot from user-facing code).
#'
#' @param reprint_error Re-prints the original error message to the console. Defaults to `FALSE`.
#' @param ggtrace_notes Prints the `ggtrace_inspect_args()` call used to inspect the error context. Defaults to `TRUE`.
#'
#' @seealso [ggtrace_inspect_on_error()]
#'
#' @return An dynamically constructed and evaluated call to [ggtrace_inspect_args()].
#' Prioritizes showing the state of layer data whenever possible (by extracting the `data` argument).
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' erroring_barplot1 <- ggplot(mtcars, aes(mpg, hp)) +
#'   stat_summary(fun.data = "mean_se") +
#'   geom_bar()
#'
#' # Render to trigger error
#' erroring_barplot1
#'
#' # Both return the same snapshot of layer data
#' # but at different levels of specificity
#' last_layer_errorcontext()
#' last_sublayer_errorcontext()
#'
#' erroring_barplot2 <- ggplot(mtcars, aes(mpg, hp)) +
#'   stat_summary() +
#'   geom_bar(aes(y = c(1, 2)))
#' erroring_barplot2
#'
#' # This works:
#' last_layer_errorcontext()
#' # This doesn't: there's no sub-layer ggproto involved in this error
#' last_sublayer_errorcontext()
#'
#' library(ggforce)
#' erroring_sina <- ggplot(mtcars, aes(mpg)) +
#'   geom_bar() +
#'   geom_sina()
#' erroring_barplot1
#'
#' # The two return different snapshots of layer data here -
#' # see `ggplot2:::Layer$compute_statistic` for why.
#' last_layer_errorcontext()
#' last_sublayer_errorcontext()
#'
#' }
last_layer_errorcontext <- function(reprint_error = FALSE, ggtrace_notes = TRUE) { # nocov start
  p <- eval.parent(rlang::call2(call("::", rlang::sym("ggplot2"), rlang::sym("last_plot"))))
  tr <- rlang::last_trace()

  layer_i <- gsub("Error occurred in the (\\d+).. layer.", "\\1", tr$body[1])

  if (layer_i == tr$body[1]) {
    stop('Error type not supported. Expected "Error occured in the {n}th layer."')
  } else {
    layer_i <- as.integer(layer_i)
  }

  callstack <- tr$trace$call %||% tr$parent$trace$call
  by_layer_frame <- which(callstack == quote(f(l = layers[[i]], d = data[[i]])))
  layer_method_call <- callstack[[by_layer_frame + 1]]
  layer_method <- rlang::call_args(layer_method_call[[1]])
  layer_i_obj <- p$layers[[layer_i]]
  layer_methods_inheritance <- get_method_inheritance(layer_i_obj)
  layer_obj <- names(Filter(function(x) any(x == rlang::as_name(layer_method[[2]])), layer_methods_inheritance))
  ns <- gsub("^namespace:", "", utils::getAnywhere("Layer")$where[1])
  layer_obj_namespaced <- call(":::", rlang::sym(ns), rlang::sym(layer_obj))
  method_expr <- call("$", layer_obj_namespaced, layer_method[[2]])

  ggtrace_ns <- resolve_ns("ggtrace")
  ggtrace_expr <- rlang::call2(
    "inspect_args",
    quote(p),
    method_expr,
    rlang::call2("layer_is", layer_i, .ns = ggtrace_ns),
    error = TRUE,
    .ns = ggtrace_ns
  )

  print_opts <- c("suppressWarnings", "suppressMessages")
  ggtrace_expr_print <- Reduce(rlang::call2, print_opts, ggtrace_expr, right = TRUE)
  if (reprint_error) {
    out <- rlang::eval_bare(ggtrace_expr_print)
    cli::cli_rule("{.pkg ggtrace} notes")
  } else {
    utils::capture.output(out <- rlang::eval_bare(ggtrace_expr_print))
  }

  if (ggtrace_notes) {
    ggtrace_expr$x <- call("last_plot")
    if ("data" %in% names(out)) {
      ggtrace_expr <- call("$", ggtrace_expr, quote(data))
      out <- asNamespace("tibble")$as_tibble(out$data)
    }
    ggtrace_expr_fmt <- rlang::expr_deparse(ggtrace_expr, width = Inf)
    cli::cli_alert_success("Executed {.code {ggtrace_expr_fmt}}")
  }

  out

} # nocov end

#' @rdname last_layer_errorcontext
#' @export
last_sublayer_errorcontext <- function(reprint_error = FALSE, ggtrace_notes = TRUE) { # nocov start
  p <- eval.parent(rlang::call2(call("::", rlang::sym("ggplot2"), rlang::sym("last_plot"))))
  tr <- rlang::last_trace()

  layer_i <- gsub("Error occurred in the (\\d+).. layer.", "\\1", tr$body[1])

  if (layer_i == tr$body[1]) {
    stop('Error type not supported. Expected "Error occured in the {n}th layer."')
  } else {
    layer_i <- as.integer(layer_i)
  }

  if (!is.null(tr$trace)) stop(paste0('Expected a sub-Layer <ggproto_method> error, not "', utils::capture.output(tr$parent), '"'))
  sublayer_tr <- tr$parent
  call_expr <- tr$parent$call
  method_expr <- call_expr[[1]]
  callstack <- sublayer_tr$trace$call
  by_layer_frame <- which(callstack == quote(f(l = layers[[i]], d = data[[i]])))
  parent_call <- callstack[[which(callstack == call_expr) - 1]]
  if (rlang::is_call_simple(parent_call)) {
    stop("Expected error from a sub-Layer <ggproto_method>, not a simple call.")
  } else if (parent_call == callstack[[by_layer_frame + 1]]) {
    stop("Error does not propagate from a sub-Layer <ggproto_method>. Use `last_layer_errorcontext()` instead.")
  }
  method_contextualized <- parent_call[[1]]
  method <- rlang::eval_tidy(method_contextualized, list(self = p$layers[[layer_i]]))
  obj <- rlang::eval_tidy(method_contextualized[[2]], list(self = p$layers[[layer_i]]))
  obj_name <- names(Filter(function(x) any(x == rlang::as_name(method_expr)), get_method_inheritance(obj)))

  obj_wheres <- utils::getAnywhere(obj_name)$where
  obj_ns <- strsplit(obj_wheres[grepl("^(package|namespace):", obj_wheres)][1], ":", fixed = TRUE)[[1]]
  obj_namespaced <- call(c("package" = "::", "namespace" = ":::")[[obj_ns[1]]], rlang::sym(obj_ns[2]), rlang::sym(obj_name))
  method_formatted <- rlang::call2("$", obj_namespaced, method_expr)

  ggtrace_expr <- rlang::expr(ggtrace_inspect_on_error(x = p, method = !!method_formatted))

  print_opts <- c("suppressWarnings", "suppressMessages")
  ggtrace_expr_print <- Reduce(rlang::call2, print_opts, ggtrace_expr, right = TRUE)
  if (reprint_error) {
    out <- rlang::eval_bare(ggtrace_expr_print)
    cli::cli_rule("{.pkg ggtrace} notes")
  } else {
    utils::capture.output(out <- rlang::eval_bare(ggtrace_expr_print))
  }

  if (ggtrace_notes) {
    ggtrace_expr$x <- call("last_plot")
    ggtrace_expr$cond = out$counter
    ggtrace_expr$error = TRUE
    ggtrace_expr[[1]] = rlang::sym("ggtrace_inspect_args")
    if ("data" %in% names(out$args)) {
      ggtrace_expr <- call("$", ggtrace_expr, quote(data))
      out <- asNamespace("tibble")$as_tibble(out$args$data)
    } else {
      out <- out$args
    }
    ggtrace_expr_fmt <- rlang::expr_deparse(ggtrace_expr, width = Inf)
    cli::cli_alert_success("Executed {.code {ggtrace_expr_fmt}}")
  }

  out

} # nocov end
