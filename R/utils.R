split_ggproto_method <- function(method_expr) {
  method_deparsed <- rlang::as_label(rlang::enexpr(method_expr))
  if (!grepl("\\$", method_deparsed)) {
    rlang::abort("Invalid method expression. See `?ggbody` for valid forms.")
  }
  both <- strsplit(method_deparsed, split = "$", fixed = TRUE)[[1]]
  obj_expr <- rlang::parse_expr(both[[1]])
  split_list <- list(
    method_name = both[[2]],
    obj = eval(obj_expr),
    obj_name = both[[1]],
    ns = gsub("(^|:::?)[^:]*?$", "", method_deparsed)
  )
  split_list$formatted_call <- paste0(split_list[["obj_name"]], "$", split_list[["method_name"]])
  split_list
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
