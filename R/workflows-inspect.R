#' Inspect the return value of a method
#'
#' @param x A ggplot object
#' @inheritParams get_method
#' @param cond When the return value should be replaced
#'
#' @return If nothing is returned, `NULL`
#' @export
#'
ggtrace_inspect_return <- function(x, method, cond = TRUE) {

  return_value <- NULL
  wrapper_env <- rlang::current_env()

  method_quo <- rlang::enquo(method)
  method_info <- resolve_formatting(method_quo)
  what <- method_info$what
  where <- method_info$where
  suppressMessages(trace(what = what, where = where, print = FALSE, exit = rlang::expr({
    if (!!cond) {
      rlang::env_bind(!!wrapper_env, return_value = returnValue())
      suppressMessages(untrace(what = !!what, where = !!where))
    }
  })))

  ggeval_silent(x)

  return_value

}
