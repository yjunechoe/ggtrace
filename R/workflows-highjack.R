#' Highjack a method's execution and modify its argument values
#'
#' @param x A ggplot object
#' @inheritParams get_method
#' @param cond When the return value should be replaced. Defaults to `1L`.
#' @param values A named list of variable-value pairings.
#'   When values are expressions, they are evaluated in the formals.
#' @param ... Unused.
#' @param draw Whether to draw the modified graphical output from evaluating `x`.
#'   Defaults to `TRUE`.
#'
#' @inheritSection topic-tracing-context Tracing context
#'
#' @return A gtable object with class `<ggtrace_highjacked>`
#' @export
#'
#' @examples
#'
#' set.seed(1116)
#' library(ggplot2)
#' library(dplyr)
#'
#'
#' p <- ggplot(mtcars, aes(mpg, hp, color = factor(cyl))) +
#'   geom_point() +
#'   geom_smooth(method = "lm")
#' p
#'
#' # Fit predictions from loess regression just for second group
#' highjack_args(
#'   x = p,
#'   method = StatSmooth$compute_group,
#'   cond = quote(data$group[1] == 2),
#'   values = list(method = loess)
#' )
#'
#' # If value is an expression, it's evaluated in the Tracing Context
#' highjack_args(
#'   x = p,
#'   method = StatSmooth$compute_group,
#'   values = rlang::exprs(
#'
#'     # Every time the method is called, call it with a bigger CI
#'     level = ._counter_ * 0.3,
#'
#'     # Fit models to just a random sample of the data
#'     data = data %>%
#'       slice_sample(prop = .8)
#'
#'   )
#' )
#'
ggtrace_highjack_args <- function(x, method, cond = 1L, values, ..., draw = TRUE) {

  rlang::check_dots_empty()

  cond <- resolve_cond(cond, multiple = TRUE)

  wrapper_env <- rlang::current_env()
  ._counter_ <- 0L

  method_quo <- rlang::enquo(method)
  method_info <- resolve_method(method_quo)
  what <- method_info$what
  where <- method_info$where
  suppressMessages(trace(what = what, where = where, print = FALSE, exit = rlang::expr({

    new_counter <- rlang::env_get(!!wrapper_env, "._counter_") + 1L
    rlang::env_bind(!!wrapper_env, ._counter_ = new_counter)

    cur_env <- rlang::current_env()

    cond <- rlang::eval_tidy(quote(!!cond), list(._counter_ = new_counter), cur_env)

    if (rlang::is_true(cond)) {
      cur_fn <- rlang::frame_fn(frame = cur_env)
      method_args <- names(formals(cur_fn))

      if ("..." %in% method_args) {
        method_args <- method_args[method_args != "..."]
        args_vals <- c(mget(method_args, cur_env), list(...))
      } else {
        args_vals <- mget(method_args, cur_env)
      }

      values <- lapply(
        quote(!!values), rlang::eval_tidy,
        data = list(._counter_ = new_counter),
        env = rlang::as_environment(args_vals, rlang::global_env())
      )

      if (!all(names(values) %in% names(args_vals))) {
        rlang::abort("Attempted to change the value of a non-existent argument")
      }

      frames <- sys.frames()
      frame_matches <- which(sapply(frames, function(env) identical(env, cur_env)))
      return_frame <- frames[[max(frame_matches[-length(frame_matches)]) - 1L]]

      # inline modify_list()
      for (i in names(values)) args_vals[[i]] <- values[[i]]

      return_value <- do.call(attr(cur_fn, "original"), args_vals)
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


#' Highjack a method's execution and make it return a user-supplied value
#'
#' @param x A ggplot object
#' @inheritParams get_method
#' @param cond When the return value should be replaced. Defaults to `1L`.
#' @param value What the method should return instead. Defaults to `quote(returnValue())`.
#' @param ... Unused.
#' @param draw Whether to draw the modified graphical output from evaluating `x`.
#'   Defaults to `TRUE`.
#'
#' @inheritSection topic-tracing-context Tracing context
#'
#' @return A gtable object with class `<ggtrace_highjacked>`
#' @export
#'
#' @examples
#'
#' set.seed(1116)
#' library(ggplot2)
#' library(dplyr)
#'
#'
#' p1 <- ggplot(diamonds, aes(cut)) +
#'   geom_bar(aes(fill = cut)) +
#'   facet_wrap(~ clarity)
#'
#' p1
#'
#' # Highjack `Stat$compute_panel` at the first panel
#' # to return higher values for `count`
#' highjack_return(
#'   x = p1, method = Stat$compute_panel,
#'   value = quote({
#'     returnValue() %>%
#'       mutate(count = count * 100)
#'   })
#' )
#'
#' # Highjack `Stat$compute_panel` at the fourth panel
#' # to shuffle bars in the x-axis
#' highjack_return(
#'   x = p1, method = Stat$compute_panel,
#'   cond = quote(data$PANEL[1] == 4),
#'   value = quote({
#'     returnValue() %>%
#'       mutate(x = sample(x))
#'   })
#' )
ggtrace_highjack_return <- function(x, method, cond = 1L, value = quote(returnValue()),
                                    ..., draw = TRUE) {

  rlang::check_dots_empty()

  cond <- resolve_cond(cond, multiple = TRUE)

  wrapper_env <- rlang::current_env()
  ._counter_ <- 0L

  method_quo <- rlang::enquo(method)
  method_info <- resolve_method(method_quo)
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
