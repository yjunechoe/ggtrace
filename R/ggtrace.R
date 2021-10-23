#' Programmatically debug ggproto methods with trace
#'
#' @inheritParams ggbody
#' @param trace_steps A sorted numeric vector of positions in the method's body to trace. Negative indices
#'   reference steps from the last, where `-1` references the last step in the body.
#' @param trace_exprs A list of expressions to evaluate at each position specified
#'   in `trace_steps`. If a single expression is provided, it is recycled to match the length of `trace_steps`.
#'
#'   To simply run a step and return its output, you can use the `~step` keyword. If the step
#'   is an assign expression, the value of the assigned variable is returned.
#'   If `trace_exprs` is not provided, `ggtrace()` is called with `~step` by default.
#'
#' @param once Whether to `untrace()` the method on exit. If `FALSE`, creates a persistent trace which is
#'   active until `gguntrace()` is called on the method. Defaults to `TRUE`.
#' @param use_names Whether the trace dump should use the names from `trace_exprs`. Defaults to `TRUE`.
#' @param print_output Whether to print the output of each expression to the console. Defaults to `TRUE`.
#' @param verbose Whether logs should be printed when trace is triggered. Encompasses `print_output`,
#'   meaning that `verbose = FALSE` also triggers the effect of `print_output = FALSE` by consequence.
#'
#' @details `ggtrace()` is a wrapper around `base::trace()` which is called on the ggproto method.
#'  It calls `base::untrace()` on itself on exit by default, to make its effect ephemeral like `base::debugonce()`.
#'  A major feature is the ability to pass multiple positions and expressions to `trace_steps` and `trace_exprs` to
#'  inspect, capture, and modify the run time environment of ggproto methods. It is recommended to consult the output
#'  of `ggbody()` when deciding which expressions to evaluate at which steps.
#'
#'  The output of the expressions passed to `trace_exprs` is printed while tracing takes place. The
#'  list of outputs from `ggtrace()` ("trace dumps") can be returned for further inspection with
#'  `last_ggtrace()` or `global_ggtrace()`.
#'
#' @section Workflows:
#'   Broadly, there are four flavors of working with the `{ggtrace}` package, listed in the order of increasing complexity:
#'
#'  - **Inspect**: The canonical use of `ggtrace()` to make queries, where expressions are passed in and
#'    their evaluated output are returned, potentially for further inspection.
#'
#'  - **Capture**: The strategy of returning the method's runtime environment for more complex explorations outside of the debugging context.
#'    A method's environment contextualizes the `self` object in addition to making all inherited params and local variables available.
#'
#'    A reference to the method's runtime environment can be returned with `environment()`, as in `trace_exprs = quote(environment())`.
#'    Note that environments are mutable, meaning that `environment()` returned from the first and last steps will reference
#'    the same environment. To get a snapshot of the environment at a particular step, you can return a deep copy with
#'    `rlang::env_clone(environment())`.
#'
#'  - **Inject**: The strategy of modifying the behavior of a method as it runs by passing in expressions that make assignments.
#'
#'    For example, `trace_steps = c(1, 10)` with `trace_exprs = rlang::exprs(a <- 5, a)` will first assign a new variable `a`
#'    at step 1, and return its value `5` at step 10. This can also be used to modify important variables like
#'    `quote(data$x <- data$x * 10)`. If you would like to inject an object from the global environment, you can make use of the
#'    `!!` (bang-bang) operator from `{rlang}`, like so: `rlang::expr(data <- !!modified_data)`.
#'
#'    Note that the execution environment is created anew each time the method is ran, so modifying the
#'    environment from its previous execution will not affect future calls to the method.
#'
#'    If you would like to capture the modified plot output and assign it to a variable, you can do so with
#'    `ggplotGrob()`. You can then render the modified plot with `print()`.
#'
#'  - **Edit**: It is also possible to make any arbitrary modifications to the method's source code, which stays in effect
#'    until the method is untraced. While this is also handled with `base::trace()`, this workflow is fundamentally
#'    interactive. Therefore, it has been refactored as its own function `ggedit()`. See `?ggedit` for more details.
#'
#' @section Gotchas:
#'
#'  - If you wrap a ggplot in `invisible()` to silence `ggtrace()`, the plot will not build, which also means that
#'    the tracing is not triggered. This is because the print/plot method of ggplot is what triggers the evaluation
#'    of the plot code. It is recommended to allow `ggtrace()` to print information, but if you'd really like to silence
#'    it, you can do so by wrapping the plot in a function that forces its evaluation first, like `ggplotGrob`,
#'    as in `invisible(ggplotGrob(<plot>))`.
#'
#'  - If for any reason `ggtrace(once = TRUE)` fails to untrace itself on exit, you may accidentally trigger
#'    the trace again. To check if a method is being traced, call `is_traced()`. You can also always call
#'    `gguntrace()` since unlike `base::untrace()`, it will not error if a trace doesn't exist on the method.
#'    Instead, `gguntrace()` will do nothing in that case and simply inform you that there is no trace to remove.
#'
#'  - Because `base::trace()` wraps the method body in a special environment, it is not possible to inspect the
#'    method/function which called it, even with something like `rlang::caller_env()`. You will traverse through
#'    a few wrapping environments created by `base::trace()` which eventually ends up looping around.
#'
#' @section Messages:
#'  Various information is sent to the console whenever a trace is triggered. You can control what gets displayed with `print_output` and
#'  `verbose`, which are both `TRUE` by default. `print_output` simply calls `print()` on the evaluated expressions, and turning this
#'  off may be desirable if expressions in `trace_exprs` evaluates to a long dataframe or vector. `verbose` controls all
#'  information printed to the console including those by `print()`, and setting `verbose = FALSE` will mean that only
#'  `message()`s will be displayed. Lastly, you can suppress `message()` from `ggtrace()` with `options(ggtrace.suppressMessages = TRUE)`,
#'  though suppressing messages is generally not recommended.
#'
#'
#' @seealso [gguntrace()], [is_traced()], [last_ggtrace()], [global_ggtrace()]
#'
#' @return NULL
#' @export
#'
#' @examples
#' # One example of an Inspect workflow ----
#'
#' library(ggplot2)
#'
#' jitter_plot <- ggplot(diamonds[1:1000,], aes(cut, depth)) +
#'   geom_point(position = position_jitter(width = 0.2, seed = 2021))
#'
#' jitter_plot
#'
#' ggbody(PositionJitter$compute_layer)
#'
#' ## Step 1 ====
#' ## Inspect what `data` look like at the start of the function
#' ggtrace(PositionJitter$compute_layer, trace_steps = 1, trace_exprs = quote(head(data)))
#'
#' jitter_plot
#'
#' ## Step 2 ====
#' ## What does `data` look like at the end of the method? Unfortunately, `trace()` only lets us enter
#' ## at the beginning of a step, so we can't inspect what happens after the last step is evaluated. To
#' ## address this, `ggtrace()` offers a `~step` keyword which gets substituted for the current line.
#' ## We also set `print_output = FALSE` to disable printing of the output
#' ggtrace(
#'   PositionJitter$compute_layer,
#'   trace_steps = 12,
#'   trace_exprs = quote(~step), # This is the default if `trace_exprs` is not provided
#'   print_output = FALSE
#' )
#'
#' # We wrap the plot in `ggplotGrob()` and `invisible()` to force
#' # its evaluation while suppressing its rendering
#' invisible(ggplotGrob(jitter_plot))
#'
#' # The output of the evaluated expressions can be inspected with `last_ggtrace()`
#' head(last_ggtrace()[[1]])
#'
#' ## Step 3 ====
#' ## If we want both to be returned at the same time for an easier comparison, we can pass in a
#' ## (named) list of expressions.
#' ggtrace(
#'   PositionJitter$compute_layer,
#'   trace_steps = c(1, 12),
#'   trace_exprs = rlang::exprs(
#'     before_jitter = data,
#'     after_jitter = ~step
#'   ),
#'   verbose = FALSE
#' )
#'
#' invisible(ggplotGrob(jitter_plot))
#'
#' ## Step 4 ====
#' ## The output of the evaluated expressions can be inspected with `last_ggtrace()`
#' jitter_tracedump <- last_ggtrace()
#'
#' lapply(jitter_tracedump, head, 3)
#'
#' jitter_distances <- jitter_tracedump[["before_jitter"]]$x -
#'   jitter_tracedump[["after_jitter"]]$x
#'
#' range(jitter_distances)
#' jitter_plot$layers[[1]]$position$width
#'
ggtrace <- function(method, trace_steps, trace_exprs, once = TRUE, use_names = TRUE, print_output = TRUE, verbose = TRUE) {

  # Capture method expression
  method_quo <- rlang::enquo(method)

  # Validate method
  method_body <- ggbody(method_quo)

  # Error if not a method
  if (class(method_body) != "list" || !all(vapply(method_body, rlang::is_expression, logical(1)))) {
    rlang::abort("Cannot trace a non-function method.")
  }

  # Parse/deparse method and obj
  method_split <- split_ggproto_method(method_quo)
  method_name <- method_split[["method_name"]]
  obj <- method_split[["obj"]]
  obj_name <- method_split[["obj_name"]]
  formatted_call <- method_split[["formatted_call"]]

  # Ensure method is untraced and body is extracted from untraced method
  if (.is_traced(method_name, obj)) {
    suppressMessages(untrace(method_name, where = obj))
    method_body <- ggbody(method_quo)
  }

  ## Number of steps
  n_steps <- length(trace_steps)

  ## Ensure `trace_exprs` is a list of expressions (recycle)
  if (rlang::is_missing(trace_exprs)) {
    trace_exprs <- rep(list(rlang::expr(~step)), n_steps)
  } else if (!is.list(trace_exprs)) {
    trace_exprs <- rep(list(trace_exprs), n_steps)
  } else if (is.list(trace_exprs) && length(trace_exprs) == 1) {
    trace_exprs <- rep(trace_exprs, n_steps)
  }

  ## Ensure `trace_exprs` is the same length as `trace_steps`
  if (length(trace_exprs) != n_steps) { rlang::abort("Length mismatch between `trace_steps` and `trace_exprs`") }

  ## Ensure trace_steps is within bounds
  trace_steps[trace_steps < 0] <- 1 + length(method_body) + trace_steps[trace_steps < 0]
  if (any(trace_steps <= 0 | trace_steps > length(method_body))) { rlang::abort("`trace_steps` out of range") }

  ## Ensure `trace_steps` is sorted
  if (!identical(trace_steps, sort.int(trace_steps))) { rlang::abort("`trace_steps` must be a sorted numeric vector") }

  ## Substitute `~step` keyword
  for (i in seq_len(n_steps)) {
    trace_expr_deparsed <- paste0(rlang::expr_deparse(trace_exprs[[i]], width = Inf), collapse = "\n")
    if (grepl("~step", trace_expr_deparsed)) {
      if (trace_expr_deparsed == "~step") {
        trace_exprs[[i]] <- method_body[[trace_steps[i]]]
      } else {
        step_expr_deparsed <- paste(rlang::expr_deparse(method_body[[trace_steps[i]]], width = Inf), collapse = "\n")
        step_expr_substituted <- gsub("~step", step_expr_deparsed, trace_expr_deparsed)
        trace_exprs[[i]] <- rlang::parse_expr(step_expr_substituted)
      }
    }
  }

  # Initialize trace dump for caching output
  trace_dump <- vector("list", n_steps)
  ## Make names from expressions
  trace_exprs_labels <- vapply(trace_exprs, function(x) { paste(rlang::expr_deparse(x), collapse = "\n") }, character(1))
  ## Use names from named elements
  trace_msgs <- paste0("[Step ", trace_steps, "]> ", trace_exprs_labels)

  ## Ensure no duplicates
  if (any(duplicated(trace_msgs))) {
    rlang::abort(paste0(
      "Duplicate names or expression evaluated at the same step.\n",
      "Please make all names and/or step-expression pairs are unique."
    ))
  }

  ## Setup trace dump
  trace_idx <- 1
  if (use_names) {
    if (is.null(names(trace_exprs))) {
      trace_dump <- unname(trace_dump)
    } else {
      names(trace_dump) <- names(trace_exprs)
    }
  } else {
    names(trace_dump) <- trace_msgs
  }

  ## Resolve info display options
  silent <- getOption("ggtrace.suppressMessages")
  tibble_print <- rlang::is_installed("tibble") && getOption("ggtrace.as_tibble")

  ## ggtrace wrapper env
  wrapper_env <- rlang::current_env()

  suppressMessages(
    trace(
      what = method_name,
      where = obj,
      at = trace_steps,
      tracer = function() {

        if (trace_idx == 1 && !silent) {
          message("Triggering ", if (!once) "persistent ", "trace on ", formatted_call)
        }

        trace_print <- gsub("\\n", "\n ", trace_msgs[trace_idx])

        # Evaluate and store output to trace dump
        trace_expr <- trace_exprs[[trace_idx]]
        trace_result <- eval(rlang::expr({
          if (!!verbose) {
            cat("\n", !!trace_print, "\n", sep = "")
            if (!!print_output) { print(!!trace_expr) }
          }
          return(!!trace_expr)
        }), envir = parent.frame()) # This is needed to escape the debugging environment

        # Resolve tibble format
        if (tibble_print && is.data.frame(trace_result)) {
          trace_result <- asNamespace("tibble")$as_tibble(trace_result)
        }

        # Accumulate (and handle NULL)
        if (is.null(trace_result)) {
          trace_result <- list(trace_result)
          names(trace_result) <- names(trace_dump)[trace_idx]
          trace_dump[trace_idx] <- trace_result
        } else {
          trace_dump[[trace_idx]] <- trace_result
        }

        # Continue
        if (trace_idx == length(trace_exprs)) {
          # Set `last_ggtrace()`
          set_last_ggtrace(trace_dump)
          # Update `global_ggtrace()`
          trace_dump_list <- list(trace_dump)
          names(trace_dump_list) <- paste(formatted_call, rlang::env_label(environment()), sep = "-")
          add_global_ggtrace(trace_dump_list)
        } else {
          trace_idx <<- trace_idx + 1
        }

        # Store output
        trace_dump <<- trace_dump

      },
      print = FALSE,
      exit = rlang::expr({
        # Check if number of actual and expected traced steps match with delayed eval
        if (rlang::env_get(!!wrapper_env, "trace_idx") < !!n_steps) {
          rlang::warn("Trace failed - fewer than expected steps. Did the method return or break early?")
        }
        # Reset idx in the closure in case of persistent trace
        rlang::env_bind(!!wrapper_env, trace_idx = 1)
        ## Messages
        if (!!verbose) { cat("\nCall `last_ggtrace()` to get the trace dump.\n") }
        if (!!once) {
          suppressMessages(untrace(!!method_name, where = !!obj))
          if (isFALSE(!!silent)) { message("Untracing ", !!formatted_call, " on exit.") }
        }
      })
    )
  )

  if (!silent) { message(formatted_call, " now being traced.") }
  if (!silent && !once) { message("Creating a persistent trace. Remember to `gguntrace(", formatted_call, ")`!") }
  invisible(NULL)

}
