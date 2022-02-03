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
  # error handling for closures
  if (!rlang::is_environment(split_list$obj)) {
    rlang::abort("The lhs of `$` must be an environment")
  }
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
    rlang::abort(call = NULL, paste0(
      "Method '", method_name, "' is not defined for `", obj_name, "`",
      "\nCheck inheritance with `get_method(", obj_name, "$", method_name, ", inherit = TRUE)`"
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

    # Error if not evaluatable
    evalled <- rlang::eval_tidy(method_quo)

    # Error if not a method
    if (typeof(method_body) != "list" | class(evalled) != "ggproto_method" | method_body[[1]] != rlang::expr(`{`)) {
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
    fn_env <- rlang::get_env(fn_call)
    what <- gsub("^.*:", "", deparsed)
    where <- rlang::get_env(fn_call)
    formatted_call <- deparsed

    # Error if not a function
    if (!rlang::is_function(fn_call)) { rlang::abort("Cannot trace a non-function.") }

    # Ensure the function is not being traced and re-evaluate fn_call
    traced <- "functionWithTrace" %in% class(get(what, envir = fn_env))
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
