#' Inspect how many times a method was called
#'
#' @param x A ggplot object
#' @inheritParams get_method
#' @param ... Unused.
#' @param error If `TRUE`, continues inspecting the method until the ggplot errors.
#'   This is useful for debugging but note that it can sometimes return incomplete output.
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
#' inspect_n(p1, Stat$compute_layer)
#'
#' # 8 calls to Stat$compute_panel
#' inspect_n(p1, Stat$compute_panel)
#'
#' # Note that there are 0 calls to Stat$compute_group ...
#' inspect_n(p1, Stat$compute_group)
#'
#' # because StatCount has its own "compute_group" method defined
#' inspect_n(p1, StatCount$compute_group)
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
#' inspect_n(p2, Stat$compute_layer)
#' inspect_n(p2, Stat$compute_panel)
#' inspect_n(p2, StatCount$compute_group)
ggtrace_inspect_n <- function(x, method, ..., error = FALSE) {

  rlang::check_dots_empty()

  wrapper_env <- rlang::current_env()
  ._counter_ <- 0L

  method_quo <- rlang::enquo(method)
  method_info <- resolve_method(method_quo)
  what <- method_info$what
  where <- method_info$where
  suppressMessages(trace(what = what, where = where, print = FALSE, tracer = rlang::expr({
    rlang::env_bind(!!wrapper_env, ._counter_ = rlang::env_get(!!wrapper_env, "._counter_") + 1L)
  })))

  simulate_plot(x, error)

  suppressMessages(untrace(what = what, where = where))

  ._counter_

}


#' Inspect which calls to a ggproto method met a particular condition
#'
#' @param x A ggplot object
#' @inheritParams get_method
#' @param cond Expression evaluating to a logical inside `method` when `x` is evaluated.
#' @param ... Unused.
#' @param error If `TRUE`, continues inspecting the method until the ggplot errors.
#'   This is useful for debugging but note that it can sometimes return incomplete output.
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
#' inspect_which(p1, StatCount$compute_group, quote(data$PANEL[1] == 2))
#'
#'
#' # How about if we add a second layer that uses StatCount?
#' p2 <- p1 + geom_text(
#'   aes(label = after_stat(count)),
#'   stat = StatCount, position = position_nudge(y = 500)
#' )
#' p2
#'
#' inspect_which(p2, StatCount$compute_group, quote(data$PANEL[1] == 2))
#'
#'
#' # Behaves like `base::which()` and returns `integer(0)` when no matches are found
#' inspect_which(p2, StatBoxplot$compute_group, quote(data$PANEL[1] == 2))
#'
ggtrace_inspect_which <- function(x, method, cond, ..., error = FALSE) {

  rlang::check_dots_empty()

  wrapper_env <- rlang::current_env()
  ._counter_ <- 0L
  indices <- integer(0)

  method_quo <- rlang::enquo(method)
  method_info <- resolve_method(method_quo)
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

  simulate_plot(x, error)

  suppressMessages(untrace(what = what, where = where))

  indices

}


#' Inspect the value of variables from a method
#'
#' @param x A ggplot object
#' @inheritParams get_method
#' @param cond When the return value should be inspected. Defaults to `1L`.
#' @param at Which steps in the method body the values of `vars` should be retrieved.
#'   Defaults to a special value `all` which is evaluated to all steps in the method body.
#' @param vars A character vector of variable names
#' @param by_var Boolean that controls the format of the output:
#'   - `TRUE` (default): returns a list of variables, with their values at each step. This
#'     also drops steps within a variable where the variable value has not changed from a previous
#'     step specified by `at`.
#'   - `FALSE`: returns a list of steps, where each element holds the value of `vars`
#'     at each step of `at`. Unchanged variable values are not dropped.
#' @param ... Unused.
#' @param error If `TRUE`, continues inspecting the method until the ggplot errors.
#'   This is useful for debugging but note that it can sometimes return incomplete output.
#'
#' @inheritSection topic-tracing-context Tracing context
#'
#' @return A list of values of `vars` at each step `at`. Simplifies if `vars` and/or `at` is length-1.
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#'
#' p1 <- ggplot(mtcars[1:10,], aes(mpg, hp)) +
#'   geom_smooth()
#' p1
#'
#' # The `data` variable is bound to two unique values in `compute_group` method:
#' inspect_vars(p1, StatSmooth$compute_group, vars = "data")
#'
#' # Note that elements of this list capture the method's state upon entering a step,
#' # so "Step1" and "Step5" should be interpreted as the value of `data` at the start
#' # the method's execution (before "Step1") and its value as a result of running Step4
#' # (before "Step5"). Indeed, we see that the `weight` column is defined in Step4, so
#' # the data is flagged as changed at the start of Step5
#' ggbody(StatSmooth$compute_group)[[4]]
#'
#'
#' # Comparing the "Steps" themselves can be useful
#' p2 <- p1 +
#'   scale_x_continuous(trans = "log") +
#'   scale_y_continuous(trans = "log")
#' p2
#'
#' # Comparing the original plot to one with log-transformed scales reveals a change
#' # in data detected at the beginning of Step 14
#' names(inspect_vars(p1, ggplot2:::ggplot_build.ggplot, vars = "data"))
#' names(inspect_vars(p2, ggplot2:::ggplot_build.ggplot, vars = "data"))
#'
#' # We can pinpoint the calculation of scale transformations to Step 13:
#' ggbody(ggplot2:::ggplot_build.ggplot)[[13]]
#'
#'
#' # With `by_vars = FALSE`, elements of the returned list are steps instead of values.
#' # Note that this does not drop unchanged values:
#' inspect_vars(p1, StatSmooth$compute_group, vars = "data", at = 1:6, by_var = FALSE)
#'
#'
ggtrace_inspect_vars <- function(x, method, cond = 1L, at = "all", vars, by_var = TRUE,
                                 ..., error = FALSE) {

  rlang::check_dots_empty()

  cond <- resolve_cond(cond)

  wrapper_env <- rlang::current_env()
  ._counter_ <- 1L

  .values <- .ggtrace_placeholder

  method_quo <- rlang::enquo(method)
  method_info <- resolve_method(method_quo)
  what <- method_info$what
  where <- method_info$where

  if (at[1] == "all") {
    at <- seq_len(length(method_info$method_body))
  } else {
    at <- replace(at, at < 0L, length(method_info$method_body) + 1L + at[at < 0L])
    if (any(at > length(method_info$method_body))) { rlang::abort("Argument `at` is out of range") }
    at <- sort(at)
  }

  suppressMessages(
    trace(what = what, where = where, print = FALSE, at = at,
          tracer = rlang::expr({

            cur_env <- rlang::current_env()
            cond <- rlang::eval_tidy(
              quote(!!cond),
              list(._counter_ = rlang::env_get(!!wrapper_env, "._counter_")),
              cur_env
            )

            if (rlang::is_true(cond)) {

              bindings <- rlang::env_get_list(cur_env, !!vars, default = structure(list(), class = "ggtrace_placeholder"))

              rlang::env_bind(!!wrapper_env,
                              .values = c(rlang::env_get(!!wrapper_env, ".values"), list(bindings)))

            } else if (!rlang::is_false(cond)) {
              rlang::warn(paste0("`cond` did not evaluate to TRUE or FALSE at `._counter_ == ",
                                 rlang::env_get(!!wrapper_env, "._counter_"), "`"))
            }

          }),
          exit = rlang::expr({
            rlang::env_bind(!!wrapper_env, ._counter_ = rlang::env_get(!!wrapper_env, "._counter_") + 1L)
            if (!is.null(rlang::env_get(!!wrapper_env, ".values"))) {
              suppressMessages(untrace(what = !!what, where = !!where))
            }
          })
    )
  )

  simulate_plot(x, error)

  if (.is_traced(what, where) || is.ggtrace_placeholder(.values)) {
    if (.is_traced(what, where)) {
      suppressMessages(untrace(what = what, where = where))
    }
    rlang::abort(paste0("No values detected from `", method_info$formatted_call,
                        "` for the given `at` and `cond` during execution of the plot"))
  }

  .values <- stats::setNames(.values, paste0("Step", at))
  .values <- lapply(.values, function(x) { Filter(function(y) { !is.ggtrace_placeholder(y) }, x) })
  present_vars <- unique(unlist(lapply(.values, names), use.names = FALSE))

  if (!all(vars %in% present_vars)) {
    rlang::warn("Bindings missing for some elements of `vars`")
    logged_vars <- vars[vars %in% present_vars]
  } else {
    logged_vars <- vars
  }

  if (by_var) {
    vars_uniques <- lapply(logged_vars, function(v) {
      which_defined <- vapply(.values, function(x) {
        ( v %in% names(x) ) && ( !is.ggtrace_placeholder(x[[v]]) )
      }, logical(1))
      v_defined <- lapply(.values[which_defined], `[[`, v)
      v_pairs <- matrix(c(1L, seq_len(length(v_defined) - 1L), seq_along(v_defined)), nrow = 2, byrow = TRUE)
      which_unchanged <- apply(v_pairs, 2, function(p) identical(v_defined[[p[1]]], v_defined[[p[2]]]))
      which_unchanged[1] <- FALSE
      v_defined[!which_unchanged]
    })
    names(vars_uniques) <- logged_vars
    .values <- vars_uniques
  }

  if (length(at) == 1 && length(.values) == 1) {
    .values[[1]][[1]]
  } else if (length(.values) == 1) {
    .values[[1]]
  } else {
    .values
  }

}


#' Inspect the arguments passed into a method
#'
#' @param x A ggplot object
#' @inheritParams get_method
#' @param cond When the arguments should be inspected. Defaults to `1L`.
#' @param hoist_dots Whether treat arguments passed to `...` like regular arguments. If `FALSE`,
#'   the `...` is treated as an argument
#' @param ... Unused.
#' @param error If `TRUE`, continues inspecting the method until the ggplot errors.
#'   This is useful for debugging but note that it can sometimes return incomplete output.
#'
#' @inheritSection topic-tracing-context Tracing context
#'
#' @return A list of argument-value pairs from the `method` when it is called.
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
#' # Argument value of `Stat$compute_panel` for the first panel
#' compute_panel_args_1 <- inspect_args(x = p1, method = Stat$compute_panel)
#' names(ggformals(Stat$compute_panel))
#' names(compute_panel_args_1)
#' table(compute_panel_args_1$data$fill)
#'
#' # `hoist_dots` preserves information about which arguments were passed to `...`
#' with_dots <- inspect_args(p1, Stat$compute_panel, hoist_dots = FALSE)
#' names(with_dots)
#' with_dots$`...`
#'
ggtrace_inspect_args <- function(x, method, cond = 1L, hoist_dots = TRUE,
                                 ..., error = FALSE) {

  rlang::check_dots_empty()

  cond <- resolve_cond(cond)

  wrapper_env <- rlang::current_env()
  ._counter_ <- 0L
  ._args <- .ggtrace_placeholder

  method_quo <- rlang::enquo(method)
  method_info <- resolve_method(method_quo)
  what <- method_info$what
  where <- method_info$where
  suppressMessages(trace(what = what, where = where, at = 1L, print = FALSE, tracer = rlang::expr({
    new_counter <- rlang::env_get(!!wrapper_env, "._counter_") + 1L
    rlang::env_bind(!!wrapper_env, ._counter_ = new_counter)
    cond <- rlang::eval_tidy(
      quote(!!cond),
      list(._counter_ = new_counter),
      rlang::current_env()
    )
    if (cond) {
      cur_fn <- attr(rlang::current_fn(), "original")
      args <- names(formals(cur_fn))
      if ("..." %in% args) {
        args_pairs <- c(as.list(mget(args[args != "..."])), list(`...` = list(...)))
      } else {
        args_pairs <- mget(args)
      }
      rlang::env_bind(!!wrapper_env, ._args = args_pairs)
      suppressMessages(untrace(what = !!what, where = !!where))
    }
  })))

  simulate_plot(x, error)

  if (.is_traced(what, where) || is.ggtrace_placeholder(._args)) {
    if (.is_traced(what, where)) {
      suppressMessages(untrace(what = what, where = where))
    }
    rlang::abort(paste0("No call to `", method_info$formatted_call,
                        "` detected at `cond` during execution of the plot"))
  } else {
    if (hoist_dots && "..." %in% names(._args)) {
      c(._args[names(._args) != "..."], ._args$`...`)
    } else {
      ._args
    }
  }

}


#' Inspect the return value of a method
#'
#' @param x A ggplot object
#' @inheritParams get_method
#' @param cond When the return value should be inspected. Defaults to `1L`.
#' @param ... Unused.
#' @param error If `TRUE`, continues inspecting the method until the ggplot errors.
#'   This is useful for debugging but note that it can sometimes return incomplete output.
#'
#' @inheritSection topic-tracing-context Tracing context
#'
#' @return The return value from `method` when it is called.
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
#' # Return value of `Stat$compute_panel` for the first panel
#' inspect_return(x = p1, method = Stat$compute_panel)
#'
#' # Return value for 4th panel
#' inspect_return(x = p1, method = Stat$compute_panel,
#'                        cond = 4L)
#'
#' # Return value for 4th panel, 2nd group (bar)
#' inspect_return(
#'   x = p1, method = StatCount$compute_group,
#'   cond = quote(data$PANEL[1] == 4 && data$group[1] == 2)
#' )
#'
ggtrace_inspect_return <- function(x, method, cond = 1L, ..., error = FALSE) {

  rlang::check_dots_empty()

  cond <- resolve_cond(cond)

  wrapper_env <- rlang::current_env()
  ._counter_ <- 0L
  ._return <- .ggtrace_placeholder

  method_quo <- rlang::enquo(method)
  method_info <- resolve_method(method_quo)
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
      rlang::env_bind(!!wrapper_env, ._return = returnValue(structure(list(), class = "ggtrace_placeholder")))
      suppressMessages(untrace(what = !!what, where = !!where))
    }
  })))

  simulate_plot(x, error)

  if (.is_traced(what, where) || is.ggtrace_placeholder(._return)) {
    if (.is_traced(what, where)) {
      suppressMessages(untrace(what = what, where = where))
    }
    cli::cli_abort(c(
      "!" = paste(
        "No call to `{method_info$formatted_call}` detected",
        "at `cond` during execution of the plot"
      )
    ))
  } else {
    ._return
  }

}

#' Get information about a ggproto method on error
#'
#' @param x A ggplot object
#' @inheritParams get_method
#' @param ... Unused.
#'
#' @return A list of three elements: `counter`, `args`, and `env`.
#' @export
#'
#' @examples
#' library(ggplot2)
#' erroring_barplot <- ggplot(mtcars, aes(mpg, hp)) +
#'   stat_summary() +
#'   geom_bar()
#' inspect_on_error(erroring_barplot, StatCount$setup_params)
#' inspect_on_error(erroring_barplot, ggplot2:::Layer$compute_statistic)
ggtrace_inspect_on_error <- function(x, method, ...) {

  rlang::check_dots_empty()

  wrapper_env <- rlang::current_env()
  ._env <- NULL
  ._counter_ <- 0L
  ._args <- .ggtrace_placeholder

  method_quo <- rlang::enquo(method)
  method_info <- resolve_method(method_quo)
  what <- method_info$what
  where <- method_info$where
  suppressMessages(trace(what = what, where = where, at = 1L, print = FALSE, tracer = rlang::expr({
    new_counter <- rlang::env_get(!!wrapper_env, "._counter_") + 1L
    args <- names(formals(attr(rlang::current_fn(), "original")))
    if ("..." %in% args) {
      args_pairs <- c(as.list(mget(args[args != "..."])), list(`...` = list(...)))
    } else {
      args_pairs <- mget(args)
    }
    rlang::env_bind(
      !!wrapper_env,
      ._counter_ = new_counter,
      ._args = args_pairs,
      ._env = rlang::current_env()
    )
  })))

  utils::capture.output(simulate_plot(x, error = TRUE))
  suppressMessages(untrace(what = what, where = where))

  if (._counter_ == 0L) {
    rlang::abort(paste0("No call to `", method_info$formatted_call,
                        "` detected at `cond` during execution of the plot"))
  }

  if ("..." %in% names(._args)) {
    ._args <- c(._args[names(._args) != "..."], ._args$`...`)
  }

  ._env <- rlang::env_unbind(rlang::env_clone(._env), c("new_counter", "args", "args_pairs"))

  list(
    counter = ._counter_,
    args = ._args,
    env = ._env
  )

}
