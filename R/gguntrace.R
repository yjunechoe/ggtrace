#' Untrace a ggproto method
#'
#' For explicitly calling `untrace()` on a ggproto object.
#'
#' @inheritParams ggbody
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
  if (rlang::is_missing(obj)) {
    method_expr <- rlang::enexpr(method)
    split <- eval(rlang::expr(split_ggproto_method(!!method_expr)))
    method <- split[[1]]
    obj <- split[[2]]
  }
  suppressMessages(untrace(what = method, where = obj))
  invisible(NULL)
}
