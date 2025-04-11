#' Remove any existing traces
#'
#' Used for explicitly calling `untrace()` on a ggproto object.
#'
#' @inheritParams get_method
#' @param ... Ignored. Designed for the ease of calling this function by modifying the call to
#'   an earlier `{ggtrace}` function in interactive contexts.
#'
#' @details Unlike `base::untrace()`, there is no adverse side effect to repeatedly calling `gguntrace()`
#'   on a ggproto method. `gguntrace()` will only throw an error if the method cannot be found.
#'
#'   If the method is valid, `gguntrace()` will do one of two things:
#'   - Inform that it has successfully removed the trace (after untracing)
#'   - Inform that the there isn't an existing trace (after doing nothing)
#'
#' @seealso [ggtrace()], [ggedit()]
#'
#' @return NULL
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' gguntrace(Stat$compute_layer)
#'
#' is_traced(Stat$compute_layer)
#'
#' ggtrace(Stat$compute_layer, 1)
#'
#' is_traced(Stat$compute_layer)
#'
#' gguntrace(Stat$compute_layer)
#'
#' is_traced(Stat$compute_layer)
#'
#' gguntrace(Stat$compute_layer)
#'
gguntrace <- function(method, ...) {

  # Capture method expression
  method_quo <- rlang::enquo(method)
  if (rlang::is_quosure(method)) {
    method_quo <- method
  }

  # Resolve formatting and dump vars
  method_info <- resolve_method(method_quo, remove_trace = FALSE)
  what <- method_info$what
  where <- method_info$where
  method_body <- method_info$method_body
  formatted_call <- method_info$formatted_call
  traced <- method_info$traced

  if (!traced) {
    message("`", formatted_call, "` not currently being traced.")
  } else {
    tryCatch(
      expr = {
        suppressMessages(untrace(what = what, where = where))
        message("`", formatted_call, "` no longer being traced.")
      },
      error = function(e) { NULL }
    )
  }

  invisible(NULL)
}

