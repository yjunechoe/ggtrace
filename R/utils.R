split_ggproto_method <- function(method) {
  method_expr <- rlang::enexpr(method)
  eval_env <- parent.frame()
  if (rlang::is_quosure(method)) {
    method_expr <- rlang::quo_get_expr(method)
    eval_env <- rlang::quo_get_env(method)
  }
  method_deparsed <- rlang::as_label(method_expr)
  if (!grepl("\\$", method_deparsed)) {
    rlang::abort("Invalid method expression. See `?ggbody` for valid forms.")
  }
  both <- strsplit(method_deparsed, split = "$", fixed = TRUE)[[1]]
  obj_expr <- rlang::parse_expr(both[[1]])
  split_list <- list(
    method_name = both[[2]],
    obj = eval(obj_expr, envir = eval_env),
    obj_name = both[[1]],
    ns = gsub("(^|:::?)[^:]*?$", "", method_deparsed)
  )
  split_list$formatted_call <- paste0(split_list[["obj_name"]], "$", split_list[["method_name"]])
  split_list
}

.is_traced <- function(method_name, obj) {
  "functionWithTrace" %in% class(get(method_name, obj))
}

resolve_method <- function(got) {
  if (is.function(got)) {
    as.list(body(got))
  } else {
    got
  }
}

sanitize_get_error <- function(e, method_name, obj_name) {
  if (e$message == paste0("object '", method_name, "' not found")) {
    rlang::abort(paste0(
      "Method ", method_name, " is not defined for ", obj_name,
      "\nCheck inheritance with `ggbody(", obj_name, "$", method_name, ", inherit = TRUE)`"
    ))
  }
}

resolve_formatting <- function(method, remove_trace = FALSE) {
  method_quo <- rlang::enquo(method)
  if (rlang::is_quosure(method)) {
    method_quo <- method
  }
  deparsed <- rlang::expr_deparse(rlang::quo_get_expr(method_quo))

  # Resolve formatting
  if (grepl("\\$", deparsed)) {
    method_body <- ggbody(method_quo)

    # Error if not a method
    if (class(method_body) != "list" || !all(vapply(method_body, rlang::is_expression, logical(1)))) {
      rlang::abort("Cannot trace a non-function.")
    }

    # Parse/deparse method and obj
    method_split <- split_ggproto_method(method_quo)
    what <- method_split[["method_name"]]
    where <- method_split[["obj"]]
    formatted_call <- method_split[["formatted_call"]]

    # Ensure method is untraced and body is extracted from untraced method
    traced <- .is_traced(what, where)
    if (remove_trace && traced) {
      suppressMessages(untrace(what = what, where = where))
      method_body <- ggbody(method_quo)
    }
  } else {
    fn_call <- rlang::eval_tidy(method_quo)
    what <- gsub("^.*:", "", deparsed)
    where <- rlang::get_env(fn_call)
    formatted_call <- deparsed

    # Error if not a function
    if (!rlang::is_function(fn_call)) { rlang::abort("Cannot trace a non-function.") }

    # Ensure the function is not being traced and re-evaluate fn_call
    traced <- "functionWithTrace" %in% class(fn_call)
    if (remove_trace && traced) {
      suppressMessages(untrace(what = what, where = where))
      fn_call <- rlang::eval_tidy(method_quo)
    }

    method_body <- as.list(body(fn_call))
  }

  list(
    what = what,
    where = where,
    method_body = method_body,
    formatted_call = formatted_call,
    traced = traced
  )
}
