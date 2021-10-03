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
#' @keywords internal
last_ggtrace <- function() .store$get()
