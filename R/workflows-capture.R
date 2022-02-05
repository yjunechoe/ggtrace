#' Capture a snapshot of a ggproto method
#'
#' Returns a ggproto method as a function with arguments pre-filled to their values when it was first called
#'
#' @param x A ggplot object
#' @param ... Passed to `ggtrace()`. The `method` to capture should be specified here.
#'
#' @note For methods that take `...`, if arguments are passed to `...` in runtime, they're captured and
#'   stored in the `.dots_captured` argument of the returned function, accessible via `formals(x)$.dots_captured`.
#'   The returned function will also expose the `...`, and the defaults in `.dots_captured` will be passed
#'   to the captured method unless also provided in the `...`
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
#' p1 <- ggplot(df, aes(x = V1, y = V2)) + stat_summary(orientation = "x")
#' p1
#'
#' p1_compute_panel <- ggtrace_capture(p1, method = StatSummary$compute_panel)
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
#' p2 <- ggplot(df, aes(x = V1, y = V2)) + stat_summary(orientation = "y")
#' p2_compute_panel <- ggtrace_capture(p2, method = StatSummary$compute_panel)
#'
#' identical(p1_compute_panel(flipped_aes = TRUE), p2_compute_panel())
#'
#' # You can interactively explore with `debugonce(p2_compute_panel)`
#'
#'
#' # Note that the captured method looks slightly different if the method takes `...`
#' p3 <- ggplot(df, aes(x = V1, y = V2)) + stat_smooth() + geom_jitter()
#' p3
#'
#' p3_compute_panel <- ggtrace_capture(p3, method = Stat$compute_panel)
#'
#' # For one, the body is different
#' body(p3_compute_panel)
#'
#' # The captured method is called internally, stored in the `"inner"` attribute
#' attr(p3_compute_panel, "inner")
#'
#' # Captured arguments are again stored in the formals of the function
#' # Note that arguments passed to the `...` are promoted to function arguments
#' names(formals(p3_compute_panel))
#' names(ggformals(Stat$compute_panel))
#'
#' # It works the same otherwise - plus you get the benefit of autocomplete
#' head(p3_compute_panel())
#' head(p3_compute_panel(level = .99)[, c("ymin", "ymax")])
#' head(p3_compute_panel(flipped_aes = TRUE))
#'
#' # Interactively explore with `debugonce(attr(p3_compute_panel, "inner"))`
#'
ggtrace_capture <- function(x, ...) {

  # Local binding shenanigans to pass check
  modify_list <- .dots_captured <- NULL

  out <- with_ggtrace(
    x = x,
    ...,
    trace_steps = 1,
    trace_exprs = rlang::expr({
      cur_fn <- rlang::current_fn()
      cur_fn <- attr(cur_fn, "original") %||% cur_fn
      args <- names(formals(cur_fn))
      if ("..." %in% args) {
        dots_params <- list(...)
        args_pairs <- as.list(mget(args[args != "..."]))

        # define inner function
        args_inner <- replicate(length(args_pairs) + 1, rlang::expr())
        names(args_inner) <- c(names(args_pairs), "...")
        inner <- rlang::new_function(args_inner, body(cur_fn))

        # define outer function
        args_outer <- c(args_pairs, dots_params)
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
  if (is.null(out)) {
    rlang::abort("No function to capture - did the ggplot call the method?")
  } else {
    out[[1]]
  }
}
