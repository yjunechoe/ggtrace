#' Inspect how many times a method was called
#'
#' @param x A ggplot object
#' @inheritParams get_method
#'
#' @return The number of times `method` was called in the evaluation of `x`
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#'
#' p1 <- ggplot(diamonds, aes(cut)) +
#'   geom_bar(aes(fill = cut)) +
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


#' Inspect which calls to a ggproto method met a particular condition
#'
#' @param x A ggplot object
#' @inheritParams get_method
#' @param cond Expression evaluating to a logical inside `method` when `x` is evaluated.
#'
#' @inheritSection topic-tracing-context Tracing context
#'
#' @return The values of the tracing context variable `._counter_` when `cond` is evaluated as `TRUE`.
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#'
#' p1 <- ggplot(diamonds, aes(cut)) +
#'   geom_bar(aes(fill = cut)) +
#'   facet_wrap(~ clarity)
#' p1
#'
#'
#' # Values of `._counter_` when `compute_group` is called for groups in the second panel:
#' ggtrace_inspect_which(p1, StatCount$compute_group, quote(data$PANEL[1] == 2))
#'
#'
#' # How about if we add a second layer that uses StatCount?
#' p2 <- p1 + geom_text(
#'   aes(label = after_stat(count)),
#'   stat = StatCount, position = position_nudge(y = 500)
#' )
#' p2
#'
#' ggtrace_inspect_which(p2, StatCount$compute_group, quote(data$PANEL[1] == 2))
#'
#'
#' # Behaves like `base::which()` and returns `integer(0)` when no matches are found
#' ggtrace_inspect_which(p2, StatBoxplot$compute_group, quote(data$PANEL[1] == 2))
#'
ggtrace_inspect_which <- function(x, method, cond) {
  wrapper_env <- rlang::current_env()
  ._counter_ <- 0L
  indices <- integer(0)

  method_quo <- rlang::enquo(method)
  method_info <- resolve_formatting(method_quo)
  what <- method_info$what
  where <- method_info$where
  suppressMessages(trace(what = what, where = where, print = FALSE, tracer = rlang::expr({
    new_counter <- rlang::env_get(!!wrapper_env, "._counter_") + 1L
    rlang::env_bind(!!wrapper_env, ._counter_ = new_counter)
    cond <- rlang::eval_tidy(
      quote(!!cond),
      list(._counter_ = new_counter),
      rlang::current_env()
    )
    if (cond) {
      rlang::env_bind(!!wrapper_env, indices = c(rlang::env_get(!!wrapper_env, "indices"), new_counter))
    }
  })))

  ggeval_silent(x)
  suppressMessages(untrace(what = what, where = where))

  indices

}


#' Inspect the return value of a method
#'
#' @param x A ggplot object
#' @inheritParams get_method
#' @param cond When the return value should be inspected. Defaults to `quote(._counter_ == 1L)`.
#'
#' @inheritSection topic-tracing-context Tracing context
#'
#' @return The return value from `method` when it is first called.
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#'
#' p1 <- ggplot(diamonds, aes(cut)) +
#'   geom_bar(aes(fill = cut)) +
#'   facet_wrap(~ clarity)
#'
#' p1
#'
#' # Return value of `Stat$compute_panel` for the
#' # first panel `cond = quote(._counter_ == 1L)`
#' ggtrace_inspect_return(x = p1, method = Stat$compute_panel)
#'
#' # Return value for 4th panel
#' ggtrace_inspect_return(x = p1, method = Stat$compute_panel,
#'                        cond = quote(._counter_ == 4L))
#'
#' # Return value for 4th panel, 2nd group (bar)
#' ggtrace_inspect_return(
#'   x = p1, method = StatCount$compute_group,
#'   cond = quote(data$PANEL[1] == 4 && data$group == 2)
#' )
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
    new_counter <- rlang::env_get(!!wrapper_env, "._counter_") + 1L
    rlang::env_bind(!!wrapper_env, ._counter_ = new_counter)
    cond <- rlang::eval_tidy(
      quote(!!cond),
      list(._counter_ = new_counter),
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
