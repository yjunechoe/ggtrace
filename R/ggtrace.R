#' Programmatically debug ggproto methods with trace
#'
#' @inheritParams ggbody
#' @param trace_steps A list of positions in the method's callstack to trace.
#' @param trace_exprs A list of expressions to evaluate at each position specified
#'   in `trace_steps`. If a single expression is provided, it is recycled.
#'
#'   To simply run a step and return its output, you can use the `~step` keyword. If the step
#'   assigns a value to a local variable, the value of that local variable is returned.
#'
#' @param once Whether to `untrace()` itself on exit. Defaults to `TRUE`.
#' @param .print Whether to print the output of each expression to the console. Defaults to `TRUE`.
#'
#' @details `ggtrace()` is a wrapper around `base::trace()` which is called on the ggproto method.
#'  It calls `base::untrace()` on itself on exit by default, to make its effect ephemeral (like `base::debugonce()`).
#'  A major feature is the ability to pass multiple positions and expressions to `trace_steps` and `trace_exprs`.
#'  It is recommended to consult the output of `ggbody()` when deciding which expressions to evaluate at which steps.
#'
#'  The output of the expressions passed to `trace_exprs` is printed while tracing takes place. The last `ggtrace()`
#'  trace dump is available for further inspection with `last_ggtrace()`.
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' p <- ggplot(diamonds[1:1000,], aes(cut, depth)) +
#'   geom_point(position = position_jitter(width = 0.2, seed = 2021))
#' p
#'
#' ggbody(PositionJitter$compute_layer)
#'
#' ## Example 1 ====
#' ## Inspect what `data` look like at the start of the function
#' ggtrace(PositionJitter$compute_layer, trace_steps = 1, trace_exprs = quote(head(data)))
#' p
#'
#' ## Example 2 ====
#' ## What does `data` look like at the end of the method? Unfortunately, `trace()` only lets us enter
#' ## at the beginning of a step, so we can't inspect what happens after the last step is evaluated. To
#' ## address this, `ggtrace()` offers a `~list` keyword which gets substituted for the current line.
#' ## We also set `.print = FALSE` to disable printing of the output
#' ggtrace(
#'   PositionJitter$compute_layer,
#'   trace_steps = 12,
#'   trace_exprs = quote(~step),
#'   .print = FALSE
#' )
#' p
#'
#' ## Example 3 ====
#' ## If we want both to be returned at the same time for an easier comparison, we can pass in a list
#' ## of expressions. We use `rlang::exprs()` here to conveniently construct a list of expressions.
#' ggtrace(
#'   PositionJitter$compute_layer,
#'   trace_steps = c(1, 12),
#'   trace_exprs = rlang::exprs(data, ~step),
#'   .print = FALSE
#' )
#' p
#'
#' ## Example 4 ====
#' ## The output of the evaluated expressions can be inspected with `last_ggtrace()`
#' jitter_tracedump <- last_ggtrace()
#' lapply(jitter_tracedump, head)
#' hist(jitter_tracedump[[1]]$x - jitter_tracedump[[2]]$x)
#' }
ggtrace <- function(method, trace_steps, trace_exprs, obj, once = TRUE, .print = TRUE) {

  if (rlang::is_missing(obj)) {
    method_expr <- rlang::enexpr(method)
    split <- eval(rlang::expr(split_ggproto_method(!!method_expr)))
    method <- split[[1]]
    obj <- split[[2]]
  }

  obj_name <- rlang::as_label(obj)

  trace_n <- length(trace_steps)
  trace_dump <- vector("list", trace_n)

  if (!is.list(trace_exprs)) {
    trace_exprs <- rep(list(trace_exprs), trace_n)
  }

  method_body <- ggbody(method, obj)
  trace_exprs <- lapply(seq_len(trace_n), function(x) {
    if (rlang::as_label(trace_exprs[[x]]) == "~step") {
      step_deparsed <- paste(rlang::expr_deparse(method_body[[trace_steps[x]]], width = Inf), collapse = "")
      line_substituted <- gsub("~step", step_deparsed, rlang::as_label(trace_exprs[[x]]))
      rlang::parse_expr(line_substituted)
    } else {
      trace_exprs[[x]]
    }
  })

  names(trace_dump) <- lapply(seq_len(trace_n), function(i) {
    paste0("[Step ", trace_steps[[i]], "]> ", paste(rlang::expr_deparse(trace_exprs[[i]]), collapse = "\n"))
  })

  trace_idx <- 1

  suppressMessages(
    trace(
      what = method,
      where = obj,
      at = trace_steps,
      tracer = function() {
        if (trace_idx == 1) {
          cat("Tracing method", method, "from", obj_name, "ggproto.\n")
        }
        trace_expr <- trace_exprs[[trace_idx]]
        trace_print <- gsub("\\n", "\n ", names(trace_dump)[trace_idx])
        trace_dump[[trace_idx]] <- eval(rlang::expr({
          cat("\n", !!trace_print, "\n")
          if (!!.print) { print(!!trace_expr) }
          return(!!trace_expr)
        }), envir = parent.frame())
        trace_dump <<- trace_dump
        if (trace_idx == length(trace_exprs)) {
          set_last_ggtrace(trace_dump)
        } else {
          trace_idx <<- trace_idx + 1
        }
      },
      print = FALSE,
      exit = rlang::expr({
        cat("\n")
        if (!!once) {
          suppressMessages(untrace(!!method, where = !!obj))
          cat("Untracing method", !!method, "from", !!obj_name, "ggproto.\n")
        } else {
          message("Creating a persistent trace. Remember to `gguntrace()`!")
        }
        cat("Call `last_ggtrace()` to get the trace dump.\n")
      })
    )
  )

  invisible(NULL)

}
