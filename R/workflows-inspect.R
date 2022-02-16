#' Inspect the return value of a method
#'
#' @param x A ggplot object
#' @inheritParams get_method
#' @param cond When the return value should be replaced. Defaults to `TRUE`.
#'   Given that only one value is returned by `ggtrace_inspect_return`, the default
#'   value is the return value from the first time the method runs.
#'
#' @details The value to `cond` can be a quoted expression, which gets evaluated every
#'   time the method is called during the evaluation of `x`. If `cond` evaluates to `TRUE`
#'   multiple times (such as in the default case when `cond` is simply `TRUE`), the function
#'   gives the return value from the first time the method runs.
#'
#'   The `cond` expression is evaluated inside the method's current execution environment, with
#'   the addition of a data-masked variable `._counter` (or `.data$._counter`), which internally
#'   tracks the current index of the method. For example, `._counter == 3` targets the third time
#'   the method is called.
#'
#'   Additionally, when quoted and passed to `cond`, the function `returnValue()` accesses the
#'   current return value of the method. For example, `unique(returnValue()$PANEL) == 2` for
#'   `Stat$compute_group` targets the method when it's processing data for the second PANEL.
#'
#' @return The return value from `method` when it is first called.
#' @export
#'
ggtrace_inspect_return <- function(x, method, cond = TRUE) {

  wrapper_env <- rlang::current_env()
  ._counter <- 0
  ._return <- NULL

  method_quo <- rlang::enquo(method)
  method_info <- resolve_formatting(method_quo)
  what <- method_info$what
  where <- method_info$where
  suppressMessages(trace(what = what, where = where, print = FALSE, exit = rlang::expr({
    rlang::env_bind(!!wrapper_env, ._counter = rlang::env_get(!!wrapper_env, "._counter") + 1)
    cond <- rlang::eval_tidy(
      quote(!!cond),
      list(._counter = rlang::env_get(!!wrapper_env, "._counter")),
      rlang::current_env()
    )
    if (cond) {
      rlang::env_bind(!!wrapper_env, ._return = returnValue())
      suppressMessages(untrace(what = !!what, where = !!where))
    }
  })))

  ggeval_silent(x)

  if (.is_traced(what, where)) {
    suppressMessages(untrace(what = what, where = where))
    rlang::abort(paste0("No return detected from `", method_info$formatted_call,
                        "` during evaluation of the plot"))
  } else {
    ._return
  }

}
