#' Shorthand aliases for workflow functions
#'
#' @description
#' These functions are identical to their long-form counterparts suffixed
#' by `ggtrace_`.
#'
#' Inspect:
#' - [inspect_n()]
#' - [inspect_which()]
#' - [inspect_vars()]
#' - [inspect_args()]
#' - [inspect_return()]
#' - [inspect_on_error()]
#'
#' Capture:
#' - [capture_fn()]
#' - [capture_env()]
#'
#' Highjack:
#' - [highjack_args()]
#' - [highjack_return()]
#'
#' @name workflow-function-aliases
#' @keywords internal
NULL

#' @rdname ggtrace_inspect_n
#' @export
inspect_n <- ggtrace_inspect_n

#' @rdname ggtrace_inspect_which
#' @export
inspect_which <- ggtrace_inspect_which

#' @rdname ggtrace_inspect_vars
#' @export
inspect_vars <- ggtrace_inspect_vars

#' @rdname ggtrace_inspect_args
#' @export
inspect_args <- ggtrace_inspect_args

#' @rdname ggtrace_inspect_return
#' @export
inspect_return <- ggtrace_inspect_return

#' @rdname ggtrace_inspect_on_error
#' @export
inspect_on_error <- ggtrace_inspect_on_error

#' @rdname ggtrace_capture_fn
#' @export
capture_fn <- ggtrace_capture_fn

#' @rdname ggtrace_capture_env
#' @export
capture_env <- ggtrace_capture_env

#' @rdname ggtrace_highjack_args
#' @export
highjack_args <- ggtrace_highjack_args

#' @rdname ggtrace_highjack_return
#' @export
highjack_return <- ggtrace_highjack_return
