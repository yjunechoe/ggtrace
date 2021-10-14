#' Check if a method is being traced
#'
#' @inheritParams ggbody
#'
#' @return logical
#' @export
#'
#' @inherit gguntrace examples
is_traced <- function(method) {
  method <- rlang::enquo(method)
  info <- split_ggproto_method(method)
  "functionWithTrace" %in% class(get(info$method_name, info$obj))
}
