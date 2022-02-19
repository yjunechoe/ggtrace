#' Capture a snapshot of a method as a pre-filled function
#'
#' Returns a ggproto method as a function with arguments pre-filled to their values when it was first called
#'
#' @param x A ggplot object
#' @inheritParams get_method
#' @param cond When the method should be captured and returned as function. Defaults to `TRUE`.
#'   Given that only one value is returned by `ggtrace_capture_fn`, the default
#'   value is the return value from the first time the method runs.
#'
#' @note For functions and methods that take `...`, arguments passed to `...` are captured and
#'   promoted to function arguments. The captured values are available for inspection via `formals()`.
#'
#' @return A function
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' set.seed(47)
#' df <- as.data.frame(matrix(sample(5, 1000, TRUE), ncol = 2))
#' table(df)
#'
#' base <- ggplot(df, aes(x = V1, y = V2))
#'
#' p1 <- base + stat_summary(orientation = "x")
#' p1
#'
#' p1_compute_panel <- ggtrace_capture_fn(p1, method = StatSummary$compute_panel)
#'
#' # `p1_compute_panel` is a copy of the ggproto method
#' body(p1_compute_panel)
#' ggbody(StatSummary$compute_panel, as.list = FALSE)
#'
#' # Its arguments are pre-filled (captured at runtime)
#' formals(p1_compute_panel)
#'
#' # Runs as it should
#' p1_compute_panel()
#'
#' # You can inspect changes to its behavior outisde of ggplot
#' # For example, see what happens when aes is flipped via `orientation = "y"`
#' p1_compute_panel(flipped_aes = TRUE)
#'
#' # We confirm this output to be true when `orientation = "y"` in `stat_summary()`
#' p2 <- base + stat_summary(orientation = "y")
#' p2_compute_panel <- ggtrace_capture_fn(p2, method = StatSummary$compute_panel)
#'
#' identical(p1_compute_panel(flipped_aes = TRUE), p2_compute_panel())
#'
#' # You can interactively explore with `debugonce(p2_compute_panel)`
#'
#'
#' # Note that the captured method looks slightly different if the method takes `...`
#' p3 <- base + stat_smooth() + geom_jitter()
#' p3
#'
#' p3_compute_panel <- ggtrace_capture_fn(p3, method = Stat$compute_panel)
#'
#' # For one, the body is different
#' body(p3_compute_panel)
#'
#' # The captured method is called internally, stored in the `"inner"` attribute
#' attr(p3_compute_panel, "inner")
#'
#' # Captured argument defaults are again available for inspection via `formals()`
#' # Note that arguments passed to the `...` are promoted to function arguments
#' names(ggformals(Stat$compute_panel))
#' names(formals(p3_compute_panel))
#'
#' # It works the same otherwise - plus you get the benefit of autocomplete
#' head(p3_compute_panel())
#' head(p3_compute_panel(level = .99)[, c("ymin", "ymax")])
#' head(p3_compute_panel(flipped_aes = TRUE))
#'
#' # Interactively explore with `debugonce(attr(p3_compute_panel, "inner"))`
#'
ggtrace_capture_fn <- function(x, method, cond = quote(._counter_ == 1L)) {

  wrapper_env <- rlang::current_env()
  ._counter_ <- 0L
  captured <- NULL

  method_quo <- rlang::enquo(method)
  method_info <- resolve_formatting(method_quo)
  what <- method_info$what
  where <- method_info$where
  suppressMessages(trace(what = what, where = where, print = FALSE, at = 1L, tracer = rlang::expr({

    new_counter <- rlang::env_get(!!wrapper_env, "._counter_") + 1L
    rlang::env_bind(!!wrapper_env, ._counter_ = new_counter)

    cur_env <- rlang::current_env()

    cond <- rlang::eval_tidy(quote(!!cond), list(._counter_ = new_counter), cur_env)

    if (rlang::is_true(cond)) {

      cur_fn <- attr(rlang::current_fn(), "original")
      args <- names(formals(cur_fn))
      if ("..." %in% args) {
        dots_params <- list(...)
        args_pairs <- as.list(mget(args[args != "..."]))

        # define inner function
        args_inner <- replicate(length(args_pairs) + 1L, rlang::expr())
        names(args_inner) <- c(names(args_pairs), "...")
        inner <- rlang::new_function(args_inner, body(cur_fn))

        # define outer function
        args_outer <- c(args_pairs, dots_params) # rlang::pairlist2("..." =)
        outer <- rlang::new_function(args_outer, rlang::expr({
          cur_args <- mget(names(formals(rlang::current_fn())))
          specs <- names(cur_args) %in% names(formals(inner))
          do.call(inner, c(cur_args[specs], cur_args[!specs]))
        }))
        attr(outer, "inner") <- inner

        rlang::env_bind(!!wrapper_env, captured = outer)

      } else {
        args_pairs <- mget(args)
        args_pairlist <- do.call(rlang::pairlist2, args_pairs)
        captured_fn <- rlang::new_function(args_pairlist, body(cur_fn))
        rlang::env_bind(!!wrapper_env, captured = captured_fn)
      }

      suppressMessages(untrace(what = !!what, where = !!where))

    } else if (!rlang::is_false(cond)) {
      rlang::warn(paste0("`cond` did not evaluate to TRUE or FALSE at `._counter_ == ", new_counter, "`"))
    }
  })))

  ggeval_silent(x)

  if (.is_traced(what, where)) {
    suppressMessages(untrace(what = what, where = where))
    rlang::abort(paste0("`", method_info$formatted_call, "` was not called during evaluation of the plot"))
  } else {
    captured
  }

}

#' Capture a snapshot of a method's execution environment
#'
#' @param x A ggplot object
#' @inheritParams get_method
#' @param cond When the method should be captured and returned as function. Defaults to `TRUE`.
#'   Given that only one value is returned by `ggtrace_capture_fn`, the default
#'   value is the return value from the first time the method runs.
#' @param at Which step of the method body the environment should be captured. See `ggbody()`.
#'
#' @return An environment
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Example from https://ggplot2.tidyverse.org/reference/aes_eval.html
#' after_scale_plot <- ggplot(mpg, aes(class, hwy)) +
#'   geom_boxplot(aes(colour = class, fill = after_scale(alpha(colour, 0.4))))
#' after_scale_plot
#'
#' # `after_scale()` is resolved by `Geom$use_defaults` (at Step 6)
#'
#' before_applying <- ggtrace_capture_env(
#'   x = after_scale_plot,
#'   method = Geom$use_defaults,
#'   at = 1  # To be more specific, do `at = 6`
#' )
#' after_applying <- ggtrace_capture_env(
#'   x = after_scale_plot,
#'   method = Geom$use_defaults,
#'   at = -1  # To be more specific, do `at = 7`
#' )
#'
#' colnames(before_applying$data)
#' colnames(after_applying$data)
#'
#' library(dplyr)
#'
#' before_applying$data %>%
#'   select(any_of(c("colour", "fill")))
#' after_applying$data %>%
#'   select(any_of(c("colour", "fill")))
#'
#' identical(
#'   before_applying$data %>%
#'     select(any_of(c("colour", "fill"))) %>%
#'     mutate(fill = alpha(colour, 0.4)),       #< after_scale() logic here
#'   after_applying$data %>%
#'     select(any_of(c("colour", "fill")))
#' )
#'
ggtrace_capture_env <- function(x, method, cond = quote(._counter_ == 1), at = -1L) {

  wrapper_env <- rlang::current_env()
  ._counter_ <- 0L
  captured <- NULL

  method_quo <- rlang::enquo(method)
  method_info <- resolve_formatting(method_quo)
  what <- method_info$what
  where <- method_info$where

  if (at < 0L) { at <- length(method_info$method_body) + 1L + at }
  if (at > length(method_info$method_body)) { rlang::abort("`at` out of range") }

  suppressMessages(trace(what = what, where = where, print = FALSE, at = at, tracer = rlang::expr({

    new_counter <- rlang::env_get(!!wrapper_env, "._counter_") + 1L
    rlang::env_bind(!!wrapper_env, ._counter_ = new_counter)

    cur_env <- rlang::current_env()

    cond <- rlang::eval_tidy(quote(!!cond), list(._counter_ = new_counter), cur_env)

    if (rlang::is_true(cond)) {

      captured_env <- rlang::env_clone(rlang::current_env())
      rlang::env_bind(!!wrapper_env, captured = captured_env)
      suppressMessages(untrace(what = !!what, where = !!where))

    } else if (!rlang::is_false(cond)) {
      rlang::warn(paste0("`cond` did not evaluate to TRUE or FALSE at `._counter_ == ", new_counter, "`"))
    }
  })))

  ggeval_silent(x)

  if (.is_traced(what, where)) {
    suppressMessages(untrace(what = what, where = where))
    rlang::abort(paste0("`", method_info$formatted_call, "` was not called during evaluation of the plot"))
  } else {
    captured
  }

}

# ggtrace_capture_frame ...
# rlang::expr(list(
#   sys.function(length(sys.calls()) - 7),
#   sys.call(length(sys.calls()) - 7)
# ))
