#' Shorthand aliases for workflow functions
#'
#' @description
#' These functions are identical to their long-form counterparts suffixed
#' by `ggtrace_`.
#'
#' Inspect:
#' - `inspect_n()`
#' - `inspect_which()`
#' - `inspect_vars()`
#' - `inspect_args()`
#' - `inspect_return()`
#' - `inspect_on_error()`
#'
#' Capture:
#' - `capture_fn()`
#' - `capture_env()`
#'
#' Highjack:
#' - `highjack_args()`
#' - `highjack_return()`
#'
#' @name workflow-function-aliases
#' @keywords internal
NULL

#' @rdname workflow-function-aliases
#' @export
inspect_n <- ggtrace_inspect_n

#' @rdname workflow-function-aliases
#' @export
inspect_which <- ggtrace_inspect_which

#' @rdname workflow-function-aliases
#' @export
inspect_vars <- ggtrace_inspect_vars

#' @rdname workflow-function-aliases
#' @export
inspect_args <- ggtrace_inspect_args

#' @rdname workflow-function-aliases
#' @export
inspect_return <- ggtrace_inspect_return

#' @rdname workflow-function-aliases
#' @export
inspect_on_error <- ggtrace_inspect_on_error

#' @rdname workflow-function-aliases
#' @export
capture_fn <- ggtrace_capture_fn

#' @rdname workflow-function-aliases
#' @export
capture_env <- ggtrace_capture_env

#' @rdname workflow-function-aliases
#' @export
highjack_args <- ggtrace_highjack_args

#' @rdname workflow-function-aliases
#' @export
highjack_return <- ggtrace_highjack_return
