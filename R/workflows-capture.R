ggtrace_capture_fn <- function(x, method) {
  x_quo <- rlang::enquo(x)
  x_quo <- rlang::quo_set_expr(x_quo, rlang::expr(ggeval_silent(!!z)))
  out <- with_ggtrace(
    x = x_quo,
    method = rlang::enquo(method),
    trace_steps = -1,
    trace_exprs = rlang::expr({
      cur_fn <- rlang::current_fn()
      cur_fn <- attr(cur_fn, "original") %||% cur_fn
      rlang::new_function(
        mget(names(formals(cur_fn))),
        body(cur_fn),
        rlang::current_env()
      )
    })
  )
  out[[1]]
}
