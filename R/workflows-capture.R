#' Capture a snapshot of a ggproto method as a pre-filled function
#'
#' Returns a ggproto method as a function with arguments pre-filled to their values when it was first called
#'
#' @param x A ggplot object
#' @param ... Passed to `ggtrace()`. The `method` to capture should be specified here.
#'
#' @note For methods that take `...`, if arguments are passed to `...` in runtime, they're captured and
#'   promoted to function arguments. The captured values are available for inspection via `formals()`.
#'
#' @return A function
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' set.seed(47)
#' df <- as.data.frame(matrix(sample(5, 50, TRUE), ncol = 2))
#' df
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
#' # We confirm this output to be true when `orientation = "y"`
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
ggtrace_capture_fn <- function(x, ...) {

  # Local binding shenanigans to pass check
  modify_list <- .dots_captured <- NULL

  out <- with_ggtrace(
    x = x,
    ...,
    trace_steps = 1,
    trace_exprs = rlang::expr({
      cur_fn <- attr(rlang::current_fn(), "original")
      args <- names(formals(cur_fn))
      if ("..." %in% args) {
        dots_params <- list(...)
        args_pairs <- as.list(mget(args[args != "..."]))

        # define inner function
        args_inner <- replicate(length(args_pairs) + 1, rlang::expr())
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
        outer

      } else {
        args_pairs <- mget(args)
        args_pairlist <- do.call(rlang::pairlist2, args_pairs)
        rlang::new_function(args_pairlist, body(cur_fn))
      }
    })
  )
  out[[1]]
}

#' Capture a snapshot of a ggproto method's execution environment
#'
#' @param x A ggplot object
#' @param ... Passed to `ggtrace()`. The `method` to capture should be specified here.
#' @param at The position in the method body when the environment should be captured.
#'   Defaults to `1L`, which is at the start of the method's execution.
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
ggtrace_capture_env <- function(x, ..., at = 1L) {
  out <- with_ggtrace(
    x = x,
    ...,
    trace_steps = at,
    trace_exprs = rlang::expr(rlang::env_clone(rlang::current_env()))
  )
  out[[1]]
}

# ggtrace_capture_frame ...
# rlang::expr(list(
#   sys.function(length(sys.calls()) - 7),
#   sys.call(length(sys.calls()) - 7)
# ))
