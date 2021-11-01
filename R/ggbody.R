#' Retrieve the body of a ggproto method as a list
#'
#' @param method A function or a ggproto method.
#'   The ggproto method may be specified using any of the following forms:
#'   - `ggproto$method`
#'   - `namespace::ggproto$method`
#'   - `namespace:::ggproto$method`
#'
#' @param inherit Whether the method should be searched from its closest parent. Defaults to `FALSE`.
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
#' @section Gotchas:
#'  - If a method is being traced via `ggtrace()` or `ggedit()`, `ggbody()` will return the current _modified state_
#'    of the method. As of v0.3.5, calling `ggbody()` on a method that has a trace on it will return a warning
#'    to emphasize this fact.
#'  - When using `inherit = TRUE`, make sure that all ggproto objects from `class(ggproto)` are available (by loading
#'    the packages where they are defined, for example). Under the hood, `ggbody()` loops through the parents
#'    to search for the method, so it needs to be able to evaluate each element of `class(ggproto)` as an object.
#'
#' @return A list
#' @export
#' @examples
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
#' # As of v.0.4.0, ggbody works for functions as well
#' ggbody(sample)
#' ggtrace(sample, 1)
#' invisible(ggbody(sample))
#' is_traced(sample)
#' gguntrace(sample)
#'
ggbody <- function(method, inherit = FALSE) {

  # Capture method expression
  method_quo <- rlang::enquo(method)
  method_deparsed <- rlang::expr_deparse(rlang::quo_get_expr(method_quo))
  arg_provided <- TRUE

  # Special handling for functions
  if (!grepl("\\$", method_deparsed) && "function" %in% class(method) || !is.null(attr(method, "original"))) {
    fn_expr <- rlang::quo_get_expr(method_quo)
    # Error if it's a call that evalutes to a function that's not `::` or `:::`
    if (rlang::is_call(fn_expr) && !rlang::call_name(fn_expr) %in% c("::", ":::")) {
      rlang::abort("Invalid expression. If you mean to pass in a function, it must be a variable not a call.")
    }
    fn_deparsed <- gsub("^.*:", "", method_deparsed)
    fn_got <- get(fn_deparsed, envir = rlang::get_env(method))
    if ("functionWithTrace" %in% class(fn_got)) { # another check: !is.null(attr(method, "original"))
      rlang::warn(paste0("`", method_deparsed, "` is currently being traced"))
    }
    result <- as.list(body(fn_got))
    return(result)
  }

  if (rlang::is_quosure(method)) {
    method_quo <- method
    arg_provided <- FALSE
  }
  method_expr <- rlang::quo_get_expr(method_quo)

  # Check if method is a call
  if (!rlang::is_call(method_expr)) {
    rlang::abort("If you mean to pass in a ggproto method, `method` must be a call. See `?ggbody` for valid forms.")
  }

  # Parse/deparse method and obj
  method_split <- split_ggproto_method(method_quo)
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
              "Method '", method_name, "' not found in parents of `", obj_name, "`",
              "\nMake sure that all relevant libraries have been loaded."
            ))
          }
        }
      )
      if (!is.null(parent_method)) {
        if (parent == parents[1]) {
          message("Method '", method_name, "' is defined for `", obj_name, "`, not inherited.")
        } else {
          message("Returning `ggbody(", parent, "$", method_name, ")`")
        }
        # Inform if already being traced
        if (arg_provided && "functionWithTrace" %in% class(parent_method)) {
          rlang::warn(paste0("`", parent, "$", method_name, "` is currently being traced"))
        }
        # Break loop and return when found
        return(resolve_method(parent_method))
      }
    }
  } else {
    result <- tryCatch(
      expr = resolve_method(get(method_name, obj)),
      error = function(e) {
        sanitize_get_error(e, method_name, obj_name)
      }
    )
    # Inform if already being traced
    if (arg_provided && "functionWithTrace" %in% class(get(method_name, obj))) {
      rlang::warn(paste0("`", method_deparsed, "` is currently being traced"))
    }
    return(result)
  }
}
