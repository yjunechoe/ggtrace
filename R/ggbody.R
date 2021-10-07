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
#' # Navigating complex inheritance
#' class(GeomArcBar)
#' invisible(ggbody(GeomArcBar$default_aes, inherit = TRUE)) # self
#' invisible(ggbody(GeomArcBar$draw_panel, inherit = TRUE))  # parent
#' invisible(ggbody(GeomArcBar$draw_key, inherit = TRUE))    # grandparent
#' invisible(ggbody(GeomArcBar$draw_group, inherit = TRUE))  # top-level
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
    parents <- setdiff(class(obj), c("ggproto", "gg"))
    for (parent in parents) {
      parent_method <- tryCatch(
        expr = get(method_name, eval(rlang::parse_expr(parent))),
        error = function(e) {
          if (parent == parents[length(parents)]) {
            rlang::abort(paste0(
              "Method ", method_name, " is not inherited for ", obj_name,
              "\nMake sure that all relevant libraries have been loaded."
            ))
          }
        }
      )
      if (!is.null(parent_method)) {
        message(paste0("Returning `ggbody(", parent, "$", method_name, ")`"))
        # Break and return when found
        return(resolve_method(parent_method))
      }
    }
  } else {
    tryCatch(
      expr = resolve_method(get(method_name, obj)),
      error = function(e) {
        sanitize_get_error(e, method_name, obj_name)
      }
    )

  }
}
