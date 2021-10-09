#' Untrace a ggproto method
#'
#' For explicitly calling `untrace()` on a ggproto object.
#'
#' @inheritParams ggbody
#' @param ... Ignored. Designed for the ease of calling this function by modifying the call to an earlier `{ggtrace}` function in interactive contexts.
#'
#' @section Gotchas:
#'  - If you try to untrace a method that is not currently being traced,
#'    you will get a `could not find function "<method>"` error. This is
#'    in contrast to if you tried to untrace simple functions like in
#'    `untrace(mean)`. Future updates to `gguntrace()` will improve
#'    handling of this default behavior from `base::trace()`.
#'
#' @seealso [ggtrace()], [ggedit()]
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' # jitter_plot <- ggplot(diamonds[1:1000,], aes(cut, depth)) +
#' #   geom_point(position = position_jitter(width = 0.2, seed = 2021))
#' # ggedit(PositionJitter$compute_layer)
#' # jitter_plot
#' # gguntrace(PositionJitter$compute_layer)
#' # jitter_plot
#' }
gguntrace <- function(method, ...) {

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

  tryCatch(
    expr = {
      suppressMessages(untrace(what = method_name, where = obj))
      message(formatted_call, " no longer being traced.")
    },
    error = function(e) {
      if (!grepl("^\\{ +.doTrace\\(", deparse1(method_body[[length(method_body)]]))) {
        message(formatted_call, " not currently being traced.")
      }
    }
  )
  invisible(NULL)
}

