#' Check if a method is being traced
#'
#' @inheritParams ggbody
#'
#' @return logical
#' @export
#'
#' @examples
#' \dontrun{
#' ggtrace(Stat$compute_layer, 1)
#' is_traced(Stat$compute_layer)
#' gguntrace(Stat$compute_layer)
#' is_traced(Stat$compute_layer)
#' }
is_traced <- function(method) {
  method <- rlang::enquo(method)
  info <- split_ggproto_method(method)
  "functionWithTrace" %in% class(get(info$method_name, info$obj))
}
