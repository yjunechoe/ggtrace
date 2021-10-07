split_ggproto_method <- function(method_expr) {
  method_deparsed <- rlang::as_label(rlang::enexpr(method_expr))
  both <- strsplit(method_deparsed, split = "$", fixed = TRUE)[[1]]
  obj_expr <- rlang::parse_expr(both[[1]])
  list(
    method_name = both[[2]],
    obj = eval(obj_expr),
    obj_name = both[[1]],
    ns = gsub("(^|:::?)[^:]*?$", "", method_deparsed)
  )
}

#' Retrieve the body of a ggproto method as a list
#'
#' @param method An expression that evaluates to the ggproto method.
#'   This may be specified using any of the following forms:
#'
#'     - `ggproto$method`
#'     - `namespace::ggproto$method`
#'     - `namespace:::ggproto$method`
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
#' ggbody(StatCount$compute_group)
#'
#' # Works for ggproto in extension packages
#'
#' ggbody(ggforce::StatDelaunaySegment$compute_group)
#'
#' library(ggforce)
#' ggbody(StatBezier$compute_panel)
#'
#' # `inherit = TRUE` will return the method from the closest parent
#'
#' # ERRORS:
#' # ggbody(StatBoxplot$compute_panel)
#' ggbody(StatBoxplot$compute_panel, inherit = TRUE)
#' ggbody(Stat$compute_panel)
#'
#' ggbody(ggforce::GeomArc$draw_key, inherit = TRUE)
#'
#' }
ggbody <- function(method, inherit = FALSE) {

  # Parse/deparse method and obj
  method_expr <- rlang::enexpr(method)
  method_split <- eval(rlang::expr(split_ggproto_method(!!method_expr)))
  method_name <- method_split[["method_name"]]
  obj <- method_split[["obj"]]
  obj_name <- method_split[["obj_name"]]

  if (inherit) {
    parents <- setdiff(class(obj)[-1], c("ggproto", "gg"))
    for (parent in parents) {
      parent_method <- tryCatch(
        expr = get(method_name, eval(rlang::parse_expr(parent))),
        error = function(e) { NULL }
      )
      if (!is.null(parent_method)) {
        message(paste0("Returning `ggbody(", parent, "$", method_name, ")`"))
        # Break and return when found
        return(as.list(body(parent_method)))
      }
    }
  } else {
    as.list(body(get(method_name, obj)))
  }
}
