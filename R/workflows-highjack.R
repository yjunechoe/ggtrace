#' Highjack a method's execution and make it return a user-supplied value
#'
#' @param x A ggplot object
#' @inheritParams get_method
#' @param cond When the return value should be replaced.
#' @param value What the method should return instead
#' @param draw Whether to draw the modified graphical output from evaluating `x`.
#'   Defaults to `TRUE`.
#'
#' @return A gtable object with class `<ggtrace_highjacked>`
#' @export
#'
ggtrace_highjack_return <- function(x, method, cond = quote(._counter_ == 1), value = quote(returnValue()), draw = TRUE) {

  wrapper_env <- rlang::current_env()
  ._counter_ <- 0L

  method_quo <- rlang::enquo(method)
  method_info <- resolve_formatting(method_quo)
  what <- method_info$what
  where <- method_info$where
  suppressMessages(trace(what = what, where = where, print = FALSE, exit = rlang::expr({

    new_counter <- rlang::env_get(!!wrapper_env, "._counter_") + 1L
    rlang::env_bind(!!wrapper_env, ._counter_ = new_counter)

    cur_env <- rlang::current_env()

    cond <- rlang::eval_tidy(quote(!!cond), list(._counter_ = new_counter), cur_env)

    if (rlang::is_true(cond)) {
      frames <- sys.frames()
      frame_matches <- which(sapply(frames, function(env) identical(env, cur_env)))
      return_frame <- frames[[max(frame_matches[-length(frame_matches)]) - 1L]]

      return_value <- rlang::eval_tidy(quote(!!value), list(._counter_ = new_counter), cur_env)
      return_expr <- rlang::call2(quote(return), return_value)

      rlang::eval_bare(return_expr, return_frame)
    } else if (!rlang::is_false(cond)) {
      rlang::warn(paste0("`cond` did not evaluate to TRUE or FALSE at `._counter_ == ", new_counter, "`"))
    }
  })))

  modified <- ggeval_silent(x)

  suppressMessages(untrace(what = what, where = where))

  class(modified) <- c("ggtrace_highjacked", class(modified))
  modified

}
