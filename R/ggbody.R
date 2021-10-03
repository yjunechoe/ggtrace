split_ggproto_method <- function(x) {
  label <- rlang::as_label(rlang::enexpr(x))
  both <- strsplit(label, split = "$", fixed = TRUE)[[1]]
  list(
    both[[2]],
    eval(rlang::parse_expr(both[[1]]))
  )
}

#' Retrieve the callstack of a ggproto method as a list
#'
#' `r lifecycle::badge('experimental')`
#'
#' Essentially calls `as.list(body(get("<method>", <obj>)))` under the hood.
#'
#' @param method The method name as a string. Alternatively an expression
#'   that evaluates to the ggproto method in the form of `ggproto$method`.
#' @param obj The ggproto object. Can be omitted if the method is an expression
#'   in the form of `ggproto$method` that evalutes to the ggproto object's method.
#'
#' @note Despite the convenience of the short form which `ggbody()` also works with,
#'   the long form of retrieving the ggproto method by specifying both the method and
#'   the object separately exists for compatibility with other ways of inspecting
#'   ggproto methods.
#'
#'   For example, this works: `debugonce(get("compute_group", StatCount))`.
#'   But this fails to insert a break point: `debugonce(StatCount$compute_group)`
#'
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' longform <- ggbody("compute_group", StatCount)
#' longform
#'
#' shortform <- ggbody(StatCount$compute_group)
#' shortform
#'
#' identical(longform, shortform)
#'
#' library(ggforce)
#' ggbody(StatBezier$compute_panel)
#' }
ggbody <- function(method, obj) {
  if (rlang::is_missing(obj)) {
    method_expr <- rlang::enexpr(method)
    split <- eval(rlang::expr(split_ggproto_method(!!method_expr)))
    method <- split[[1]]
    obj <- split[[2]]
  }
  as.list(body(get(method, obj)))
}
