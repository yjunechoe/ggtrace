#' Interactively edit the source code of a ggproto method
#'
#' @inheritParams ggbody
#'
#' @note Like `base::trace()`, the edit is in place until `untrace()` is called.
#'   To `untrace()` a ggproto method, the syntax is `untrace(what = "method", where = obj)`
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' # jitter_plot <- ggplot(diamonds[1:1000,], aes(cut, depth)) +
#' #   geom_point(position = position_jitter(width = 0.2, seed = 2021))
#' # ggedit(PositionJitter$compute_layer)
#' # < interactively modify the method's source code in text editor >
#' # jitter_plot # Edit is in place
#' # untrace(what = "compute_layer", where = PositionJitter)
#' # jitter_plot # Edit is removed
#' }
ggedit <- function(method, obj) {
  if (rlang::is_missing(obj)) {
    method_expr <- rlang::enexpr(method)
    split <- eval(rlang::expr(split_ggproto_method(!!method_expr)))
    method <- split[[1]]
    obj <- split[[2]]
  }
  trace(what = method, where = obj, edit = TRUE)
  invisible(NULL)
}
