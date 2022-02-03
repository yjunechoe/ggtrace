#' Check if a method is being traced
#'
#' @inheritParams get_method
#'
#' @return logical
#' @export
#'
#' @inherit gguntrace examples
is_traced <- function(method) {
  resolve_formatting(rlang::enquo(method), remove_trace = FALSE)$traced
}


.get_method <- function(method_quo, inherit = FALSE) {

  method <- rlang::eval_tidy(method_quo)
  method_deparsed <- rlang::expr_deparse(rlang::quo_get_expr(method_quo))
  arg_provided <- TRUE

  # Special handling for functions
  if (!grepl("\\$", method_deparsed) && "function" %in% class(method) || !is.null(attr(method, "original"))) {
    fn_expr <- rlang::quo_get_expr(method_quo)
    # Error if it's a call that evalutes to a function that's not `::` or `:::`
    if (rlang::is_call(fn_expr) && !is.null(rlang::call_name(fn_expr))) {
      rlang::abort("Invalid expression. If you mean to pass in a function, it must be a variable not a call.")
    }
    fn_deparsed <- gsub("^.*:", "", method_deparsed)
    fn_got <- get(fn_deparsed, envir = rlang::get_env(method))
    if ("functionWithTrace" %in% class(fn_got)) { # another check: !is.null(attr(method, "original"))
      rlang::warn(paste0("`", method_deparsed, "` is currently being traced"))
    }
    result <- fn_got
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
          rlang::inform(paste0("Method '", method_name, "' is defined for `", obj_name, "`, not inherited."))
        } else {
          rlang::inform(paste0("Method inherited from `", parent, "$", method_name, "`"))
        }
        # Inform if already being traced
        if (arg_provided && "functionWithTrace" %in% class(parent_method)) {
          rlang::warn(paste0("`", parent, "$", method_name, "` is currently being traced"))
        }
        # Break loop and return when found
        return(parent_method)
      }
    }
  } else {
    result <- tryCatch(
      expr = get(method_name, obj),
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
