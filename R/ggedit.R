#' Interactively edit a masking copy of the source code
#'
#' @inheritParams ggtrace
#' @param remove_trace Whether to edit from a clean slate. Defaults to `FALSE`.
#'
#' @details Like `base::trace()`, the edit is in effect until `gguntrace()` is called.
#'   Changes with `ggedit()` are cumulative, so `ggedit()` will inform you via a warning
#'   if you're making an edit on top of an existing edit. Call `gguntrace()` on the object
#'   first if you'd like to edit the method's original unaltered source code.
#'
#'   Only works in interactive contexts.
#'
#' @section Gotchas:
#'
#'   - Calling `ggtrace()` on an method that that has changes from `ggedit()` will remove the
#'     changes from `ggedit()`. It _is_ possible to combine both features, but disabled in
#'     the package to keep the API consistent. It is against the philosophy of `{ggtrace}` to
#'     mix programmatic and interactive workflows.
#'
#' @seealso [gguntrace()], [is_traced()]
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#'
#' jitter_plot <- ggplot(diamonds[1:1000,], aes(cut, depth)) +
#'   geom_point(position = position_jitter(width = 0.2, seed = 2021))
#'
#' # Interactively modify the method's source code in text editor
#' ggedit(Position$compute_layer)
#'
#' # Check the edited code
#' ggbody(Position$compute_layer)
#'
#' # Execute method with edit
#' jitter_plot
#'
#' # Untrace
#' gguntrace(Position$compute_layer)
#'
#' # Edit is removed in the next call
#' jitter_plot
#'
#' }
ggedit <- function(method, remove_trace = FALSE, ...) { # nocov start

  if (interactive()) {
    # Capture method expression
    method_quo <- rlang::enquo(method)

    # Resolve formatting and dump vars
    method_info <- resolve_method(method_quo, remove_trace = FALSE)
    what <- method_info$what
    where <- method_info$where
    method_body <- method_info$method_body
    formatted_call <- method_info$formatted_call
    traced <- method_info$traced

    # Inform if editing on top of existing trace
    if (traced && !remove_trace) { message("Editing on top of existing trace...") }

    suppressMessages(trace(what = what, where = where, edit = TRUE))
    message("Creating a persistent trace on `", formatted_call, "`",
            "\nCall `gguntrace(", formatted_call,  ")` to untrace")
    invisible(NULL)
  }

} # nocov end
