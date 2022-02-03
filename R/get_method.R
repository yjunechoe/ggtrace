#' Get ggproto methods
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
#' @param as.list Whether `ggbody()` should return the body of the method as a list. Defaults to `TRUE`.
#'
#' @details
#'
#'   - `get_method()` returns the method.
#'   - `ggbody()` returns the body of the method.
#'   - `ggformals()` returns the formals of the method.
#'
#' @note `get_method()` calls `get("method", ggproto)` under the hood.
#'   The `get("method", ggproto)` syntax is the long form of `ggproto$method` which retrieves
#'   the actual function body. This is a subtle but important difference for inspecting ggproto methods.
#'
#'   - For example, this works: `debugonce(get("compute_group", StatCount))`
#'
#'   - But this fails to insert a break point: `debugonce(StatCount$compute_group)`
#'
#'   `get_method()` was designed so that you do not have to worry about this distinction.
#'
#' @section Gotchas:
#'  - If a method is being traced via `ggtrace()` or `ggedit()`, `get_method()` will return the current _modified state_
#'    of the method. As of v0.3.5, calling `get_method()` on a method that has a trace on it will return a warning
#'    to emphasize this fact.
#'  - When using `inherit = TRUE`, make sure that all ggproto objects from `class(ggproto)` are available (by loading
#'    the packages where they are defined, for example). Under the hood, `get_method()` loops through the parents
#'    to search for the method, so it needs to be able to evaluate each element of `class(ggproto)` as an object.
#'
#' @return A list
#' @export
#' @examples
#' library(ggplot2)
#'
#' # Uninformative
#' StatCount$compute_group
#' formals(StatCount$compute_group)
#' body(StatCount$compute_group)
#'
#' # Errors
#' # get(StatCount$compute_group)
#'
#' # Informative
#' get_method(StatCount$compute_group)
#' ggformals(StatCount$compute_group) # formals(get_method(StatCount$compute_group))
#' ggbody(StatCount$compute_group)    # body(get_method(StatCount$compute_group))
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
#' ## get_method(StatBoxplot$compute_panel)
#' ## ggbody(StatBoxplot$compute_panel)
#' ## ggformals(StatBoxplot$compute_panel)
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
get_method <- function(method, inherit = FALSE) {
  .get_method(rlang::enquo(method), inherit = inherit)
}

#' @export
#' @rdname get_method
ggformals <- function(method, inherit = FALSE) {
  got <- .get_method(rlang::enquo(method), inherit = inherit)
  if (rlang::is_function(got)) {
    formals(got)
  } else {
    rlang::abort("Cannot retrieve the formals of a non-function")
  }
}

#' @export
#' @rdname get_method
ggbody <- function(method, inherit = FALSE, as.list = TRUE) {
  got <- .get_method(rlang::enquo(method), inherit = inherit)
  if (rlang::is_function(got)) {
    if (as.list) {
      as.list(body(got))
    } else {
      body(got)
    }
  } else {
    got
  }
}
