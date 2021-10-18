#' Untrace a ggproto method
#'
#' For explicitly calling `untrace()` on a ggproto object.
#'
#' @inheritParams ggbody
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

  # Validate method
  method_body <- ggbody(method_quo)

  # Parse/deparse method and obj
  method_split <- split_ggproto_method(method_quo)
  method_name <- method_split[["method_name"]]
  obj <- method_split[["obj"]]
  obj_name <- method_split[["obj_name"]]
  formatted_call <- method_split[["formatted_call"]]

  tryCatch(
    expr = {
      suppressMessages(untrace(what = method_name, where = obj))
      message(formatted_call, " no longer being traced.")
    },
    error = function(e) {
      if (!.is_traced(method_name, obj)) {
        message(formatted_call, " not currently being traced.")
      }
    }
  )
  invisible(NULL)
}

