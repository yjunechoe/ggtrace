#' Debug a ggproto method
#'
#' @inheritParams ggbody
#' @param ... Ignored. Designed for the ease of calling this function by modifying the call to
#'   an earlier `{ggtrace}` function in interactive contexts.
#'
#' @return NULL
#' @export
ggdebug <- function(method, ...) {
  debug(get_method(rlang::enquo(method)))
}

#' @rdname ggdebug
#' @export
ggdebugonce <- function(method, ...) {
  debugonce(get_method(rlang::enquo(method)))
}

#' @rdname ggdebug
#' @export
ggundebug <- function(method, ...) {
  undebug(get_method(rlang::enquo(method)))
}
