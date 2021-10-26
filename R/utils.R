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

split_generic_method <- function(method) {
  method_expr <- rlang::enexpr(method)
  eval_env <- parent.frame()
  if (rlang::is_quosure(method)) {
    method_expr <- rlang::quo_get_expr(method)
    eval_env <- rlang::quo_get_env(method)
  }
  method_deparsed <- rlang::as_label(method_expr)
  if (!grepl("\\.", method_deparsed)) {
    rlang::abort("Invalid method expression for a S3/S4 generic.")
  }
  split_loc <- regexpr("\\.[^\\.]*$", method_deparsed)[1]
  method_generic <- substr(method_deparsed, 0, split_loc - 1)
  method_class <- substr(method_deparsed, split_loc + 1, nchar(method_deparsed))
  ns <- gsub("(^|:::?)[^:]*?$", "", method_deparsed)
  fn <- gsub("^.+:", "", method_generic)
  if (!ns %in% loadedNamespaces()) {
    rlang::abort(paste0("Package {", ns, "} must be loaded"))
  }
  defined <- rownames(attr(utils::methods(fn), "info"))
  method_full <- gsub("^.+:", "", method_deparsed)
  if (!method_full %in% defined) {
    rlang::abort(paste0("Method '", fn,"' not defined for class \"", method_class, "\""))
  }
  list(
    method_generic = method_generic,
    method_class = method_class,
    method_ns = ns,
    method_full = method_full,
    formatted_call = method_deparsed
  )
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
