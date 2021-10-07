split_ggproto_method <- function(x) {
  label <- rlang::as_label(rlang::enexpr(x))
  both <- strsplit(label, split = "$", fixed = TRUE)[[1]]
  obj_expr <- rlang::parse_expr(both[[1]])
  list(
    method = both[[2]],
    obj = eval(obj_expr),
    obj_name = rlang::as_label(obj_expr)
  )
}

#' Retrieve the body of a ggproto method as a list
#'
#' @param method The method name as a string. Alternatively an expression
#'   that evaluates to the ggproto method in the form of `ggproto$method`.
#' @param obj The ggproto object asn an expression. Can be omitted if the method is an
#'   expression in the form of `ggproto$method` that evalutes to the object's method.
#'
#' @param inherit Whether the method should be returned from its closest parent.
#'   Defaults to `FALSE`.
#'
#' @details For interactive uses, using the short form is recommended. For
#'   programmatic uses, `as.list(body(get("method", ggproto)))` is recommended.
#'
#'   The `get("method", ggproto)` syntax is the long form of `ggproto$method` with
#'   subtle but important differences;
#'
#'   - For example, this works: `debugonce(get("compute_group", StatCount))`
#'
#'   - But this fails to insert a break point: `debugonce(StatCount$compute_group)`
#'
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # Methods can be specified in both long form and short form
#'
#' longform <- ggbody("compute_group", StatCount)
#' longform
#'
#' shortform <- ggbody(StatCount$compute_group)
#' shortform
#'
#' identical(longform, shortform)
#'
#' # Works for ggproto in extension packages
#'
#' library(ggforce)
#' ggbody(StatBezier$compute_panel)
#'
#' # `inherit = TRUE` will return the method from the closest parent
#'
#' ggbody(StatBoxplot$compute_panel, inherit = TRUE)
#' ggbody(Stat$compute_panel)
#'
#' }
ggbody <- function(method, obj, inherit = FALSE) {

  # Parse/deparse method and obj
  if (rlang::is_missing(obj)) {
    method_expr <- rlang::enexpr(method)
    method_split <- eval(rlang::expr(split_ggproto_method(!!method_expr)))
    method <- method_split[["method"]]
    obj <- method_split[["obj"]]
    obj_name <- method_split[["obj_name"]]
  } else {
    obj_name <- rlang::as_string(rlang::enexpr(obj))
  }

  if (inherit) {
    parents <- setdiff(class(obj)[-1], c("ggproto", "gg"))
    for (parent in parents) {
      parent_method <- tryCatch(
        expr = get(method, eval(rlang::parse_expr(parent))),
        error = function(e) { NULL }
      )
      if (!is.null(parent_method)) {
        message(paste0("Returning `ggbody(", parent, "$", method, ")`"))
        # Break and return when found
        return(as.list(body(parent_method)))
      }
    }
  } else {
    as.list(body(get(method, obj)))
  }
}
