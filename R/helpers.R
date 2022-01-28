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

#' Return ggproto methods as functions
#'
#' @inheritParams ggbody
#'
#' @return Function
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Uninformative
#' StatCount$compute_group
#' formals(StatCount$compute_group)
#' body(StatCount$compute_group)
#'
#' # Errors
#' # get(StatCount$compute_group)
#'
#' # Informative
#' get_method(StatCount$compute_group)
#' formals(get_method(StatCount$compute_group))
#' body(get_method(StatCount$compute_group))
get_method <- function(method) {
  method_quo <- rlang::enquo(method)
  if (rlang::is_quosure(method)) {
    method_quo <- method
  }
  method_info <- resolve_formatting(method_quo)
  get(method_info$what, envir = method_info$where)
}
