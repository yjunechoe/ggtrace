#' Untrace a ggproto method
#'
#' For explicitly calling `untrace()` on a ggproto object.
#'
#' @inheritParams ggbody
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
gguntrace <- function(method, obj) {

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

  suppressMessages(untrace(what = method, where = obj))
  message(paste("Removed tracing on", method, "from", obj_name))
  invisible(NULL)
}

