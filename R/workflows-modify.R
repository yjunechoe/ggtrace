#' Modify the return value of a method
#'
#' @param x A ggplot object
#' @inheritParams get_method
#' @param value Value for the method to return when it is called
#' @param cond When the return value should be replaced
#' @param draw Whether to draw the modified plot
#'
#' @return A gtable object with class `<ggtrace_modified>`
#' @export
#'
ggtrace_modify_return <- function(x, method, value, cond = TRUE, draw = TRUE) {

  method_quo <- rlang::enquo(method)
  method_info <- resolve_formatting(method_quo)
  what <- method_info$what
  where <- method_info$where
  suppressMessages(trace(what = what, tracer = rlang::expr({
    if (!!cond) {
      # print(list(sys.calls(), sys.frames(), sys.calls()[[(sys.nframe() - 5L)]]))
      rlang::eval_bare(rlang::expr(return(!!value)), sys.frames()[[(sys.nframe() - 5L)]])
    }
  }), print = FALSE, where = where))

  modified <- ggeval_silent(x)

  suppressMessages(untrace(what = what, where = where))

  class(modified) <- c("ggtrace_modified", class(modified))
  modified

}
