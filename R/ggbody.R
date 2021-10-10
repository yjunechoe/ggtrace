#' Retrieve the body of a ggproto method as a list
#'
#' @param method An expression that evaluates to the ggproto method.
#'   This may be specified using any of the following forms:
#'
#'     - `ggproto$method`
#'
#'     - `namespace::ggproto$method`
#'
#'     - `namespace:::ggproto$method`
#'
#' @param inherit Whether the method should be returned from its closest parent. Defaults to `FALSE`.
#'   If `TRUE`, returns the parent's method and the corresponding `ggbody()` code as a message.
#'
#' @details `ggbody()` calls `as.list(body(get("method", ggproto)))` under the hood.
#'   The `get("method", ggproto)` syntax is the long form of `ggproto$method` which retrieves
#'   the actual function body. This is a subtle but important difference for inspecting ggproto methods.
#'
#'   - For example, this works: `debugonce(get("compute_group", StatCount))`
#'
#'   - But this fails to insert a break point: `debugonce(StatCount$compute_group)`
#'
#'   `ggbody()` was designed so that you do not have to worry about this distinction.
#'
#' @return A list
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
#' ## ERRORS:
#' ## ggbody(StatBoxplot$compute_panel)
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
#' # Works for custom ggproto
#' # - Example from {ggplot2} "Extending ggplot2" vignette
#' StatDensityCommon <- ggproto("StatDensityCommon", Stat,
#'   required_aes = "x",
#'
#'   setup_params = function(data, params) {
#'     if (!is.null(params$bandwidth))
#'       return(params)
#'
#'     xs <- split(data$x, data$group)
#'     bws <- vapply(xs, bw.nrd0, numeric(1))
#'     bw <- mean(bws)
#'     message("Picking bandwidth of ", signif(bw, 3))
#'
#'     params$bandwidth <- bw
#'     params
#'   },
#'
#'   compute_group = function(data, scales, bandwidth = 1) {
#'     d <- density(data$x, bw = bandwidth)
#'     data.frame(x = d$x, y = d$y)
#'   }
#' )
#'
#' as.list(body(get("compute_group", StatDensityCommon)))
#'
#' ggbody(StatDensityCommon$compute_group)
#'
#' }
ggbody <- function(method, inherit = FALSE) {

  # Capture method expression
  method_expr <- rlang::enexpr(method)

  # Check if method is a call
  if (!rlang::is_call(method_expr)) {
    rlang::abort("`method` must be a call. See `?ggbody` for valid forms.")
  }

  # Parse/deparse method and obj
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
        if (parent == parents[1]) {
          message("Method ", method_name, " is defined for ", obj_name, ", not inherited.")
        } else {
          message("Returning `ggbody(", parent, "$", method_name, ")`")
        }
        # Break loop and return when found
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
