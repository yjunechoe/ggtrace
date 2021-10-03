.ggtrace_store <- function() {
  .last_ggtrace <- NULL
  list(
    get = function() .last_ggtrace,
    set = function(value) .last_ggtrace <<- value
  )
}
.store <- .ggtrace_store()

set_last_ggtrace <- function(value) .store$set(value)

#' Retrieve the trace dump created by the last ggtrace
#'
#' @export
#' @rdname ggtrace
#' @keywords internal
last_ggtrace <- function() .store$get()

split_ggproto_method <- function(x) {
  label <- rlang::as_label(rlang::enexpr(x))
  both <- strsplit(label, split = "$", fixed = TRUE)[[1]]
  list(
    both[[2]],
    eval(rlang::parse_expr(both[[1]]))
  )
}

#' Return the callstack of a ggproto method as a list
#'
#' @param method The method name as a string. Alternatively an expression
#'   that evaluates to the ggproto method in the form of `ggproto$method`.
#' @param obj The ggproto object. Can be omitted if the method is an expression
#'   in the form of `ggproto$method` that evalutes to the ggproto object's method.
#'
#' @export
#' @rdname ggtrace
ggbody <- function(method, obj) {
  if (rlang::is_missing(obj)) {
    method_expr <- rlang::enexpr(method)
    split <- eval(rlang::expr(split_ggproto_method(!!method_expr)))
    method <- split[[1]]
    obj <- split[[2]]
  }
  as.list(body(get(method, obj)))
}

#' Trace a ggproto method
#'
#' @param method The method name as a string. Alternatively an expression
#'   that evaluates to the ggproto method in the form of `ggproto$method`.
#' @param obj The ggproto object. Can be omitted if the method is an expression
#'   in the form of `ggproto$method` that evalutes to the ggproto object's method.
#' @param trace_steps A list of positions in the method's callstack to trace.
#' @param trace_exprs A list of expressions to evaluate at each position specified
#'   in `trace_steps`. If a single expression is provided, it is recycled.
#' @param .print Whether to print the output of each expression to the console.
#'
#' @details `ggtrace()` is a wrapper around `base::trace()` which is called on the ggproto method.
#'  It calls `base::untrace()` on itself on exit by design, so its effect is ephemeral (like `base::debugonce()`).
#'  A major feature is the ability to pass multiple positions and expressions to `trace_steps` and `trace_exprs`.
#'  It is recommended to consult the output of `ggbody()` when deciding which expressions to evaluate at which steps.
#'
#'  The output of the expressions passed to `tracce_exprs` is printed while tracing takes place. The last `ggtrace()`
#'  trace dump is available for further inspection with `last_ggtrace()`.
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' # `ggbody()` can be used to get the ggproto method's callstack as a list ----
#'
#' ## You can pass in the ggproto method to `ggbody()` in two ways:
#' longform <- ggbody("compute_group", StatCount)
#' shortform <- ggbody(StatCount$compute_group)
#' identical(longform, shortform)
#' longform
#'
#' ## Essentially, ggbody does the following under the hood:
#' ## - `as.list(body(get("compute_group", StatCount)))`
#' ## This long form of retrieving the ggproto method by specifying both the
#' ## method and object separately is for compatibility with other ways
#' ## of inspecting ggproto methods.
#' ## - For example, this works: `debugonce(get("compute_group", StatCount))`
#' ## - But this doesn't place a breakpoint: `debugonce(StatCount$compute_group)`
#'
#' # `ggtrace()` allows you to debug ggproto methods PROGRAMMATICALLY. ----
#'
#' ## Comparisons with other ways of debugging/inspecting:
#' ## --- <coming soon as a vignette, but sneak peak>:
#' ## --- debug()/debugonce(), browser(), trace(), layer_data()
#'
#' ## After inspecting the ggproto method with `ggbody`, you can specify
#' ## which expression(s) to evaluate where, in `trace_steps` and `trace_exprs`.
#'
#' ## Here's a ggplot to demonstrate. Let's imagine that we'd like to inspect PositionJitter
#' p <- ggplot(diamonds[1:1000,], aes(cut, depth)) +
#'   geom_point(position = position_jitter(width = 0.2, seed = 2021))
#' p
#' ggbody(PositionJitter$compute_layer)
#'
#' ## Example 1 ====
#' ## Inspect what `data` look like at the start of the function
#' ggtrace(PositionJitter$compute_layer, trace_steps = 1, trace_exprs = rlang::expr(head(data)))
#' p
#'
#' ## Example 2 ====
#' ## What does `data` look like at the end of the method? Unfortunately, `trace()` only lets us enter
#' ## at the beginning of a step, so we can't inspect what happens after the last step is evaluated. To
#' ## address this, `ggtrace()` offers a `~list` keyword which gets substituted for the current line.
#' ggtrace(PositionJitter$compute_layer, trace_steps = 12, trace_exprs = quote(head(~line)))
#' p
#'
#' ## Example 3 ====
#' ## If we want both to be returned at the same time for an easier comparison, we can pass in a list
#' ## of expressions. We use `rlang::exprs()` here to conveniently construct a list of expressions.
#' ggtrace(
#'   PositionJitter$compute_layer,
#'   trace_steps = c(1, 1, 12),
#'   trace_exprs = rlang::exprs(
#'     head(data),
#'     params,
#'     head(~line)
#'   )
#' )
#' p
#'
#' ## Example 4 ====
#' ## We've been using `head()` for cleaner printing,
#' ## but we can also disable this with `.print = FALSE`
#' ggtrace(
#'   PositionJitter$compute_layer,
#'   trace_steps = c(1, 12),
#'   trace_exprs = rlang::exprs(data, ~line),
#'   .print = FALSE
#' )
#' p
#'
#' ## Example 5 ====
#' ## The output of the evaluated expressions can be inspected with `last_ggtrace()`
#' jitter_tracedump <- last_ggtrace()
#' lapply(jitter_tracedump, head)
#' hist(jitter_tracedump[[1]]$x - jitter_tracedump[[2]]$x)
#' }
ggtrace <- function(method, obj, trace_steps, trace_exprs, .print = TRUE) {

  if (rlang::is_missing(obj)) {
    method_expr <- rlang::enexpr(method)
    split <- eval(rlang::expr(split_ggproto_method(!!method_expr)))
    method <- split[[1]]
    obj <- split[[2]]
  }

  obj_name <- rlang::as_label(obj)

  trace_n <- length(trace_steps)
  trace_dump <- vector("list", trace_n)

  if (!is.list(trace_exprs)) {
    trace_exprs <- rep(list(trace_exprs), trace_n)
  }

  names(trace_dump) <- purrr::map2_chr(trace_steps, trace_exprs, ~ {
    paste0("[Step ", .x, "]> ", paste(rlang::expr_deparse(.y), collapse = "\n"))
  })

  method_body <- ggbody(method, obj)
  trace_exprs <- purrr::map(seq_len(trace_n), ~ {
    if (grepl("~line", rlang::as_label(trace_exprs[[.x]]))) {
      step_deparsed <- rlang::expr_deparse(method_body[[trace_steps[.x]]])
      line_substituted <- gsub("~line", step_deparsed, rlang::as_label(trace_exprs[[.x]]))
      rlang::parse_expr(line_substituted)
    } else {
      trace_exprs[[.x]]
    }
  })

  trace_idx <- 1

  suppressMessages(
    trace(
      what = method,
      where = obj,
      at = trace_steps,
      tracer = function() {
        if (trace_idx == 1) {
          cat("Tracing method", method, "from", obj_name, "ggproto.\n")
        }
        trace_expr <- trace_exprs[[trace_idx]]
        trace_print <- gsub("\\n", "\n ", names(trace_dump)[trace_idx])
        trace_dump[[trace_idx]] <- eval(rlang::expr({
          cat("\n", !!trace_print, "\n")
          if (!!.print) { print(!!trace_expr) }
          return(!!trace_expr)
        }), envir = parent.frame())
        trace_dump <<- trace_dump
        if (trace_idx == length(trace_exprs)) {
          set_last_ggtrace(trace_dump)
        } else {
          trace_idx <<- trace_idx + 1
        }
      },
      print = FALSE,
      exit = rlang::expr({
        cat("\n");
        suppressMessages(untrace(!!method, where = !!obj));
        cat("Untracing method", !!method, "from", !!obj_name, "ggproto.\n");
        cat("Call `last_ggtrace()` to get the trace dump.\n")
      })
    )
  )

  invisible(NULL)

}
