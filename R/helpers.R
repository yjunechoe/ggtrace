#' Check if a method is being traced
#'
#' @inheritParams ggbody
#'
#' @return logical
#' @export
#'
#' @inherit gguntrace examples
is_traced <- function(method) {
  resolve_formatting(rlang::enquo(method), remove_trace = FALSE)$traced
}

