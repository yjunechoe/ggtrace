#' Interactively edit the source code of a ggproto method
#'
#' @inheritParams ggbody
#'
#' @details Like `base::trace()`, the edit is in place until `untrace()` is called.
#'   To `untrace()` a ggproto method, the syntax is `untrace(what = "method", where = obj)`
#'
#' @seealso [gguntrace()]
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' # jitter_plot <- ggplot(diamonds[1:1000,], aes(cut, depth)) +
#' #   geom_point(position = position_jitter(width = 0.2, seed = 2021))
#' # ggedit(PositionJitter$compute_layer)
#' # # < interactively modify the method's source code in text editor >
#' # jitter_plot # Edit is in place
#' # gguntrace(PositionJitter$compute_layer)
#' # # Or untrace(what = "compute_layer", where = PositionJitter)
#' # jitter_plot # Edit is removed
#' }
ggedit <- function(method, obj) {

  # Parse/deparse method and obj
  if (rlang::is_missing(obj)) {
    method_expr <- rlang::enexpr(method)
    method_split <- eval(rlang::expr(split_ggproto_method(!!method_expr)))
    method <- method_split[["method"]]
    obj <- method_split[["obj"]]
    obj_name <- method_split[["obj_name"]]
  } else {
    obj_name <- rlang::as_string(rlang::enexpr(obj))
  }

  suppressMessages(trace(what = method, where = obj, edit = TRUE))
  message("Creating a persistent trace on ", method, " from ", obj_name,
          "\nCall `gguntrace(", obj_name, "$", method,  ")` to untrace")
  invisible(NULL)
}

