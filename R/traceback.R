
#' @export

tb <- function(frame = NULL, ...) {
  if (is.null(frame)) {
    format_trace(data$last_dump)
  } else {
    if (!is_count(frame) || frame > length(data$last_dump$calls)) {
      message("\nInvalid frame: ", frame)
      return(invisible())
    }

    trace_code(data$last_dump, frame, ...)
  }
}
