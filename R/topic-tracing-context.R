#' Context-dependent workflow expressions in the tracing context
#'
#' @name topic-tracing-context
#'
#' @section Tracing context:
#' When quoted expressions are passed to the `cond` or `value` argument of
#' workflow functions they are evaluated in a special environment which
#' we call the "tracing context".
#'
#' The tracing context is "data-masked" (see `rlang::eval_tidy()`), and exposes
#' an internal variable called `._counter_` which increments every time a
#' function/method has been called by the ggplot object supplied to the `x`
#' argument of workflow functions. For example, `cond = quote(._counter_ == 1L)`
#' is evaluated as `TRUE` when the method is called for the first time. The
#' `cond` argument also supports numeric shorthands like `cond = 1L` which evaluates to
#' `quote(._counter_ == 1L)`, and this is the default value of `cond` for
#' all workflow functions that only return one value (e.g., `ggtrace_capture_fn()`).
#' It is recommended to consult the output of `ggtrace_inspect_n()` and
#' `ggtrace_inspect_which()` to construct expressions that condition on `._counter_`.
#'
#' For highjack functions like `ggtrace_highjack_return()`, the value about to
#' be returned by the function/method can be accessed with `returnValue()` in the
#' `value` argument. By default, `value` is set to `quote(returnValue())` which
#' simply evaluates to the return value, but directly computing on `returnValue()` to
#' derive a different return value for the function/method is also possible.
#'
#' @keywords internal
NULL
