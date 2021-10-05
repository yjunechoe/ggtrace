.ggtrace_store <- function() {
  .last_ggtrace <- NULL
  list(
    get = function() .last_ggtrace,
    set = function(value) .last_ggtrace <<- value
  )
}
.store <- .ggtrace_store()

set_last_ggtrace <- function(value) .store$set(value)

#' Retrieve the trace dump created by the last `ggtrace()`
#'
#' @seealso [ggtrace()]
#'
#' @return A list
#' @export
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' ggbody(StatSmooth$compute_group)
#'
#' ggtrace(StatSmooth$compute_group, trace_steps = 12, trace_exprs = quote(head(prediction)))
#'
#' ggplot(mtcars, aes(mpg, hp)) + geom_point() + geom_smooth(method = 'lm')
#'
#' last_ggtrace()
#'
#' ggtrace(
#'   StatSmooth$compute_group,
#'   trace_steps = 12,
#'   trace_exprs = quote(prediction),
#'   .print = FALSE
#' )
#'
#' ggplot(mtcars, aes(mpg, hp)) + geom_point() + geom_smooth(method = 'lm')
#'
#' last_ggtrace()
#' }
last_ggtrace <- function() .store$get()
