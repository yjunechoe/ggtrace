#' Check if a method is being traced
#'
#' @inheritParams ggbody
#'
#' @return logical
#' @export
#'
#' @inherit gguntrace examples
is_traced <- function(method) {
  method_quo <- rlang::enquo(method)
  info <- split_ggproto_method(method_quo)
  "functionWithTrace" %in% class(get(info$method_name, info$obj))
}
