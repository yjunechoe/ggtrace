ggtrace_fn <- function(fn, trace_steps, trace_exprs, once = TRUE, use_names = TRUE, print_output = TRUE, verbose = TRUE) {

  fn_quo <- rlang::enquo(fn)
  fn_call <- rlang::eval_tidy(fn_quo)

  what <- gsub("^.*:", "", rlang::expr_deparse(rlang::quo_get_expr(fn_quo)))
  where <- rlang::get_env(fn_call)

  # Ensure get_expr evaluates to a function
  if (!rlang::is_function(fn_call)) { rlang::abort("Cannot trace a non-function.") }

  # Ensure the function is not being traced and re-evaluate fn_call
  if ("functionWithTrace" %in% class(fn_call)) {
    suppressMessages(untrace(what = what, where = where))
    fn_call <- rlang::eval_tidy(fn_quo)
  }

  method_body <- as.list(body(fn_call))

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
      what = what,
      where = where,
      at = trace_steps,
      tracer = function() {

        if (trace_idx == 1 && !silent) {
          message("Triggering ", if (!once) "persistent ", "trace on ", what)
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

        # Log if complete
        if (trace_idx == length(trace_exprs)) {
          # Set `last_ggtrace()`
          set_last_ggtrace(trace_dump)
          # Update `global_ggtrace()`
          trace_dump_list <- list(trace_dump)
          names(trace_dump_list) <- paste(what, rlang::env_label(environment()), sep = "-")
          add_global_ggtrace(trace_dump_list)
        }

        # Increment
        trace_idx <<- trace_idx + 1

        # Store output
        trace_dump <<- trace_dump

      },
      print = FALSE,
      exit = rlang::expr({
        ## Check if number of actual and expected traced steps match with delayed eval
        if (rlang::env_get(!!wrapper_env, "trace_idx") - 1 < !!n_steps) {
          rlang::warn(local({
            actual_trace_n <- rlang::env_get(!!wrapper_env, "trace_idx") - 1
            incomplete_trace_dump <- rlang::env_get(!!wrapper_env, "trace_dump")[seq_len(actual_trace_n)]
            if (length(seq_len(actual_trace_n)) == 0) { incomplete_trace_dump <- NULL }
            incomplete_trace_dump_lst <- list(incomplete_trace_dump)
            names(incomplete_trace_dump_lst) <- paste(!!what, "INCOMPLETE", sep = "-")
            rlang::exec(!!set_last_ggtrace, incomplete_trace_dump)
            rlang::exec(!!add_global_ggtrace, incomplete_trace_dump_lst)
            ## Return warning message
            paste("Trace incomplete [", "Actual:", actual_trace_n, "| Expected:", !!n_steps, "]",
                  "- Did the method return or break early?")
          }))
        }
        ## Reset idx in the closure in case of persistent trace
        rlang::env_bind(!!wrapper_env, trace_idx = 1)
        ## Messages
        if (!!verbose) { cat("\nCall `last_ggtrace()` to get the trace dump.\n") }
        if (!!once) {
          suppressMessages(untrace(what = !!what, where = !!where))
          if (isFALSE(!!silent)) { message("Untracing ", !!what, " on exit.") }
        }
      })
    )
  )

  if (!silent) { message(what, " now being traced.") }
  if (!silent && !once) { message("Creating a persistent trace on ", what, ".") }
  invisible(NULL)

}
