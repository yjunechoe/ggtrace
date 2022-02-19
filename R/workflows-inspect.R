#' Inspect how many times a method was called
#'
#' @param x A ggplot object
#' @inheritParams get_method
#'
#' @return Integer
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#'
#' p1 <- ggplot(diamonds, aes(cut)) +
#'   geom_bar() +
#'   facet_wrap(~ clarity)
#'
#' p1
#'
#' # 1 call to Stat$compute_layer
#' ggtrace_inspect_n(p1, Stat$compute_layer)
#'
#' # 8 calls to Stat$compute_panel
#' ggtrace_inspect_n(p1, Stat$compute_panel)
#'
#' # Note that there are 0 calls to Stat$compute_group ...
#' ggtrace_inspect_n(p1, Stat$compute_group)
#'
#' # because StatCount has its own "compute_group" method defined
#' ggtrace_inspect_n(p1, StatCount$compute_group)
#'
#' # How about if we add a second layer that uses StatCount?
#' p2 <- p1 + geom_text(
#'   aes(label = after_stat(count)),
#'   stat = StatCount, position = position_nudge(y = 500)
#' )
#'
#' p2
#'
#' # Now there are double the calls to Stat/StatCount methods
#' ggtrace_inspect_n(p2, Stat$compute_layer)
#' ggtrace_inspect_n(p2, Stat$compute_panel)
#' ggtrace_inspect_n(p2, StatCount$compute_group)
#'
#' # But separate calls to each layer's respective Geoms
#' # (note that Bar and Text are vectorized at the panel level)
#' ggtrace_inspect_n(p2, GeomBar$draw_panel)
#' ggtrace_inspect_n(p2, GeomText$draw_panel)
#'
ggtrace_inspect_n <- function(x, method) {

  wrapper_env <- rlang::current_env()
  ._counter_ <- 0L

  method_quo <- rlang::enquo(method)
  method_info <- resolve_formatting(method_quo)
  what <- method_info$what
  where <- method_info$where
  suppressMessages(trace(what = what, where = where, print = FALSE, tracer = rlang::expr({
    rlang::env_bind(!!wrapper_env, ._counter_ = rlang::env_get(!!wrapper_env, "._counter_") + 1L)
  })))

  ggeval_silent(x)
  suppressMessages(untrace(what = what, where = where))

  ._counter_

}


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
#'   the addition of a data-masked variable `._counter_` (or `.data$._counter_`), which internally
#'   tracks the current index of the method. For example, `._counter_ == 3` targets the third time
#'   the method is called.
#'
#'   Additionally, when quoted and passed to `cond`, the function `returnValue()` accesses the
#'   current return value of the method. For example, `unique(returnValue()$PANEL) == 2` for
#'   `Stat$compute_group` targets the method when it's processing data for the second PANEL.
#'
#' @return The return value from `method` when it is first called.
#' @export
#'
ggtrace_inspect_return <- function(x, method, cond = quote(._counter_ == 1L)) {

  wrapper_env <- rlang::current_env()
  ._counter_ <- 0L
  ._return <- NULL

  method_quo <- rlang::enquo(method)
  method_info <- resolve_formatting(method_quo)
  what <- method_info$what
  where <- method_info$where
  suppressMessages(trace(what = what, where = where, print = FALSE, exit = rlang::expr({
    rlang::env_bind(!!wrapper_env, ._counter_ = rlang::env_get(!!wrapper_env, "._counter_") + 1L)
    cond <- rlang::eval_tidy(
      quote(!!cond),
      list(._counter_ = rlang::env_get(!!wrapper_env, "._counter_")),
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
