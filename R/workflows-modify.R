#' Modify the return value of a method
#'
#' @param x A ggplot object
#' @inheritParams get_method
#' @param value Value for the method to return when it is called
#' @param cond When the return value should be replaced
#' @param draw Whether to draw the modified plot
#'
#' @return A gtable object with class `<ggtrace_modified>`
#' @export
#'
ggtrace_modify_return <- function(x, method, value, cond = TRUE, draw = TRUE) {

  wrapper_env <- rlang::current_env()
  ._counter_ <- 0

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
    }
    if (!rlang::is_false(cond)) {
      rlang::warn(paste0("`cond` did not evaluate to TRUE or FALSE at `._counter_ == ", new_counter, "`"))
    }
  })))

  modified <- ggeval_silent(x)

  suppressMessages(untrace(what = what, where = where))

  class(modified) <- c("ggtrace_modified", class(modified))
  modified

}
