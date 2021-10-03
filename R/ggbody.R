split_ggproto_method <- function(x) {
  label <- rlang::as_label(rlang::enexpr(x))
  both <- strsplit(label, split = "$", fixed = TRUE)[[1]]
  list(
    both[[2]],
    eval(rlang::parse_expr(both[[1]]))
  )
}

#' Return the callstack of a ggproto method as a list
#'
#' @param method The method name as a string. Alternatively an expression
#'   that evaluates to the ggproto method in the form of `ggproto$method`.
#' @param obj The ggproto object. Can be omitted if the method is an expression
#'   in the form of `ggproto$method` that evalutes to the ggproto object's method.
#'
#' @export
ggbody <- function(method, obj) {
  if (rlang::is_missing(obj)) {
    method_expr <- rlang::enexpr(method)
    split <- eval(rlang::expr(split_ggproto_method(!!method_expr)))
    method <- split[[1]]
    obj <- split[[2]]
  }
  as.list(body(get(method, obj)))
}
