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
#'   If `trace_exprs` is not provided, `ggtrace()` is called with `~step` by default.
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
#' @section Gotchas:
#'  - If you wrap a ggplot in `invisible()` to silence `ggtrace()`, the plot will not build, which also means that
#'    the tracing is not triggered. The print/plot method of ggplot is what triggers the evaluation of the plot
#'    code. It is recommended to allow `ggtrace()` to print messages for safety, but if you'd really like to silence
#'    it, you can do so by wrapping the plot in `invisible(capture.output(<plot>))`.
#'  - If for any reason `ggtrace(once = TRUE)` fails to untrace itself on exit, you may accidentally trigger
#'    the tracing again. To check if a method is being traced, call `ggbody()` on it and inspect its body. If you
#'    see `.doTrace()` scattered around the body, that's a sign the method is still being traced.
#'
#' @seealso [last_ggtrace()], [gguntrace()]
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
ggtrace <- function(method, trace_steps, trace_exprs, trace_cond, obj, once = TRUE, .print = TRUE) {

  # Parse `ggproto$method` into its parts
  if (rlang::is_missing(obj)) {
    method_expr <- rlang::enexpr(method)
    split <- eval(rlang::expr(split_ggproto_method(!!method_expr)))
    method <- split[[1]]
    obj <- split[[2]]
    # Get ggproto name as string
    obj_name <- rlang::as_label(obj)
  }

  # Capture trace condition
  if (rlang::is_missing(trace_cond)) {
    trace_cond <- TRUE
    trace_conditioned <- FALSE
  } else {
    trace_cond <- enexpr(trace_cond)
    trace_conditioned <- TRUE
  }

  # Initialize trace dump for caching output
  trace_n <- length(trace_steps)
  trace_dump <- vector("list", trace_n)

  # Ensure `trace_exprs` is a list of expressions
  if (rlang::is_missing(trace_exprs)) {
    trace_exprs <- rep(list(rlang::expr(~step)), trace_n)
  } else if (!is.list(trace_exprs)) {
    trace_exprs <- rep(list(trace_exprs), trace_n)
  }

  # Substitute `~step` keyword
  method_body <- ggbody(method, obj)
  trace_exprs <- lapply(seq_len(trace_n), function(x) {
    if (rlang::as_label(trace_exprs[[x]]) == "~step") {
      method_body[[trace_steps[x]]]
    } else {
      trace_exprs[[x]]
    }
  })

  # Printing step and expression to console
  names(trace_dump) <- lapply(seq_len(trace_n), function(i) {
    paste0("[Step ", trace_steps[[i]], "]> ", paste(rlang::expr_deparse(trace_exprs[[i]]), collapse = "\n"))
  })

  # A closure for controlling the flow of trace
  trace_closure <- (function() {
    trace_idx <- 1
    exit <- FALSE
    cur_env <- rlang::current_env()
    list(
      get_env = function() { cur_env },
      get_idx = function() { trace_idx },
      increment_idx = function() { trace_idx <<- trace_idx + 1 },
      finish = function(trace_dump) {
        set_last_ggtrace(trace_dump)
        exit <<- TRUE
      }
    )
  })()

  suppressMessages(
    trace(
      what = method,
      where = obj,
      at = trace_steps,
      tracer = function() {

        if (eval(trace_cond, envir = parent.frame())) {

          trace_idx <- trace_closure$get_idx()
          if (trace_idx == 1) { cat("Tracing method", method, "from", obj_name, "ggproto.\n") }

          trace_print <- names(trace_dump)[trace_idx]

          # Evaluate expression and push to trace dump
          trace_expr <- trace_exprs[[trace_idx]]
          trace_dump[[trace_idx]] <- eval(rlang::expr({
            cat("\n", !!trace_print, "\n")
            if (!!.print) { print(!!trace_expr) }
            return(!!trace_expr)
          }), envir = parent.frame())

          if (trace_idx == length(trace_exprs)) {
            trace_closure$finish(trace_dump)
          } else {
            trace_closure$increment_idx()
          }

          # carry over trace_dump
          trace_dump <<- trace_dump

        }

      },
      print = FALSE,
      exit = rlang::expr({
        cat("\n")
        if (!!once && eval(rlang::expr(exit), !!trace_closure$get_env())) {
          # If you are conditioning the trace, `once = TRUE` is necessary
          suppressMessages(untrace(!!method, where = !!obj))
          cat("Untracing method", !!method, "from", !!obj_name, "ggproto.\n")
          cat("Call `last_ggtrace()` to get the trace dump.\n")
        } else {
          # If you are using `once = FALSE`, send message
          message("Running with a persistent trace.")
        }
      })
    )
  )

  invisible(NULL)

}
