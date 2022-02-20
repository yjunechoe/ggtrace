#' Context-dependent workflow expressions in the tracing context
#'
#' @name topic-tracing-context
#'
#' @section Tracing context:
#' When quoted expressions are passed to the `cond` or `value` argument of
#' workflow functions (in the form of `ggtrace_{action}_{value}()`), they are
#' evaluated in a special environment which we call the "tracing context".
#'
#' The tracing context is "data-masked" (see `rlang::eval_tidy()`), and exposes
#' an internal variable called `._counter_` (`.data$._counter_`) which tracks how
#' many times a function/method has been called by the ggplot object supplied to
#' the `x` argument of workflow functions. For example, `cond = quote(._counter_ == 1)`
#' is evaluated as `TRUE` when the method is called for the first time, and
#' this is the default value of `cond` for workflow functions that only return
#' one value (e.g., `ggtrace_capture_fn()`).
#'
#' For highjack functions like `ggtrace_highjack_return()`, the value about to
#' be returned by the function/method can be accessed with `returnValue()` in the
#' `value` argument. By default, `value` is set to `quote(returnValue())` which
#' simply evaluates to the return value, but directly computing on `returnValue()` to
#' derive a different return value for the function/method is also possible.
#'
#' @keywords internal
NULL
