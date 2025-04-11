#' Check if a method is being traced
#'
#' @inheritParams get_method
#'
#' @return logical
#' @export
#'
#' @inherit gguntrace examples
is_traced <- function(method) {
  resolve_method(rlang::enquo(method), remove_trace = FALSE)$traced
}
