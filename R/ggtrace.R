#' Programmatically debug ggproto methods with trace
#'
#' @inheritParams ggbody
#' @param trace_steps A list of positions in the method's body to trace. Negative indices
#'   reference steps from the last, where `-1` references the last step in the body.
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
#'  A major feature is the ability to pass multiple positions and expressions to `trace_steps` and `trace_exprs` to
#'  inspect, debug, and modify the run time environment of ggproto methods. It is recommended to consult the output
#'  of `ggbody()` when deciding which expressions to evaluate at which steps.
#'
#'  The output of the expressions passed to `trace_exprs` is printed while tracing takes place. The
#'  list of outputs from the last `ggtrace()` can be returned for further inspection with `last_ggtrace()`.
#'
#' @section Tips & Tricks:
#'  - If the intent is to run complex calculations, it is recommended to use `ggtrace()` to simply return the method's
#'    run time environment with `trace_exprs = quote(environment())`. The returned environment is the method's execution
#'    environment which also contextualizes the `self` object in addition to making all local variables available. This
#'    allows for more complex explorations outside of the debugger, and is also recommended for safety reasons.
#'  - To modify the behavior of a method as it runs, you can pass in an expression that make assignments. For example,
#'    `trace_steps = c(1, 10)` with `rlang::exprs(a <- 5, a)` will first assign a new variable `a` at step 1, and then
#'    return its value `5` at step 10. This can also be used to modify important variables like `quote(data <- <expr>)`.
#'    Note that this only in effect while the method is being traced. For making any arbitrary modifications to the code,
#'    see [ggedit()]).
#'
#' @section Gotchas:
#'  - If you wrap a ggplot in `invisible()` to silence `ggtrace()`, the plot will not build, which also means that
#'    the tracing is not triggered. This is because the print/plot method of ggplot is what triggers the evaluation
#'    of the plot code. It is recommended to allow `ggtrace()` to print messages, but if you'd really like to silence
#'    it, you can do so by wrapping the plot in `invisible(capture.output(<plot_object>))`.
#'  - If for any reason `ggtrace(once = TRUE)` fails to untrace itself on exit, you may accidentally trigger
#'    the tracing again. To check if a method is being traced, call `ggbody()` on it and inspect its body. If you
#'    see `.doTrace()` scattered around the body, that's a sign the method is still being traced. You can also always
#'    `gguntrace()` any number of times without adverse consequences.
#'  - Environments are mutable, which means that returning `environment()` at different steps in the body will still
#'    reference the same run time environment. To get a snapshot of the method's environment at a particular step,
#'    it is recommended to use `rlang::env_clone(environment())` instead, which makes a deep copy.
#'      - Note that the execution environment is created anew each time the method is ran, so modifying the
#'        environment from its previous execution will not affect future calls to the method, even with `once = FALSE`.
#'      - Because `base::trace()` wraps the method body in a special environment, it is not possible to inspect the
#'        higher method which called it, even with something like `rlang::caller_env()`. You will traverse through
#'        a few enclosing environments created by `base::trace()` which eventually ends up looping around.
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
#' ## address this, `ggtrace()` offers a `~step` keyword which gets substituted for the current line.
#' ## We also set `.print = FALSE` to disable printing of the output
#' ggtrace(
#'   PositionJitter$compute_layer,
#'   trace_steps = 12,
#'   trace_exprs = quote(~step), # This the default if `trace_exprs` is not provided
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
ggtrace <- function(method, trace_steps, trace_exprs, once = TRUE, .print = TRUE) {

  # Capture method expression
  method_expr <- rlang::enexpr(method)

  # Validate method
  method_body <- eval(rlang::expr(ggbody(!!method_expr)))

  # Parse/deparse method and obj
  method_split <- eval(rlang::expr(split_ggproto_method(!!method_expr)))
  method_name <- method_split[["method_name"]]
  obj <- method_split[["obj"]]
  obj_name <- method_split[["obj_name"]]
  formatted_call <- method_split[["formatted_call"]]

  ## Number of steps
  n_steps <- length(trace_steps)

  ## Ensure `trace_exprs` is a list of expressions
  if (rlang::is_missing(trace_exprs)) {
    trace_exprs <- rep(list(rlang::expr(~step)), n_steps)
  } else if (!is.list(trace_exprs)) {
    trace_exprs <- rep(list(trace_exprs), n_steps)
  }

  ## Ensure trace_steps is within bounds
  trace_steps[trace_steps < 0] <- 1 + length(method_body) + trace_steps[trace_steps < 0]
  if (any(trace_steps <= 0 | trace_steps > length(method_body))) { rlang::abort("`trace_steps` out of range") }

  ## Substitute `~step` keyword
  for (i in seq_len(n_steps)) {
    if (rlang::as_label(trace_exprs[[i]]) == "~step") {
      trace_exprs[[i]] <- method_body[[trace_steps[i]]]
    }
  }

  # Initialize trace dump for caching output
  trace_dump <- vector("list", n_steps)
  ## Make names from expressions
  names(trace_dump) <- lapply(seq_len(n_steps), function(i) {
    paste(rlang::expr_deparse(trace_exprs[[i]]), collapse = "\n")
  })
  ## Use names from named elements
  names(trace_dump) <- lapply(seq_len(n_steps), function(i) {
    if (names(trace_exprs[i]) == "") {
      trace_name <- names(trace_dump[i])
    } else {
      trace_name <- paste0('"', names(trace_exprs[i]), '"')
    }
    paste0("[Step ", trace_steps[i], "]> ", trace_name)
  })

  # For incrementally storing results to `trace_dump`
  trace_idx <- 1

  suppressMessages(
    trace(
      what = method_name,
      where = obj,
      at = trace_steps,
      tracer = function() {

        if (trace_idx == 1) {
          cat("Triggering trace on", formatted_call, "\n")
        }

        trace_print <- gsub("\\n", "\n ", names(trace_dump)[trace_idx])

        # Evaluate and store output to trace dump
        trace_expr <- trace_exprs[[trace_idx]]
        trace_dump[[trace_idx]] <- eval(rlang::expr({
          cat("\n", !!trace_print, "\n")
          if (!!.print) { print(!!trace_expr) }
          return(!!trace_expr)
        }), envir = parent.frame()) # This is needed to escape the debugging environment

        if (trace_idx == length(trace_exprs)) {
          set_last_ggtrace(trace_dump)
        } else {
          trace_idx <<- trace_idx + 1
        }

        # Store output
        trace_dump <<- trace_dump

      },
      print = FALSE,
      exit = rlang::expr({
        cat("\nCall `last_ggtrace()` to get the trace dump.\n")
        if (!!once) {
          suppressMessages(untrace(!!method_name, where = !!obj))
          message("Untracing ", !!formatted_call)
        } else {
          message(!!formatted_call, " has a persistent trace.\n",
                  "Remember to `gguntrace(", !!formatted_call, ")`!")
        }
      })
    )
  )

  message(formatted_call, " now being traced")
  invisible(NULL)

}
