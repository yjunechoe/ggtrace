#' Debugonce a ggproto method
#'
#' @inheritParams ggbody
#' @param ... Ignored. Designed for the ease of calling this function by modifying the call to
#'   an earlier `{ggtrace}` function in interactive contexts.
#'
#' @return NULL
#' @export
ggdebugonce <- function(method, ...) {
  debugonce(get_method(rlang::enquo(method)))
}

#' Inspect the call stack on enter
#'
#' @details Calls `rlang::trace_back()` upon first entering the function or method. Note that `ggtraceback()`
#' only ever prints the back trace once because it calls `gguntrace()` after the first time it's
#' triggered. For more complex inspections of the call stack, `ggedit()` is recommended.
#'
#' @note The output of `rlang::trace_back()` is not logged to the tracedumps (`last_ggtrace()` and
#' `global_ggtrace()`)
#'
#' @inheritParams ggbody
#' @inheritDotParams rlang::trace_back
#'
#' @return NULL
#' @export
ggtraceback <- function(method, ...) {
  params <- rlang::list2(...)
  method_quo <- rlang::enquo(method)
  method_info <- resolve_formatting(method_quo)
  what <- method_info$what
  where <- method_info$where
  suppressMessages(trace(
    what = what,
    tracer = rlang::expr({
      if (rlang::eval_bare(.is_traced(!!what, !!where))) {
        print(rlang::trace_back(!!!params))
        gguntrace(!!method_quo)
      }
    }),
    print = FALSE,
    where = where
  ))
  message("Tracing ", method_info$formatted_call, " with `rlang::trace_back`")
  invisible(NULL)
}
