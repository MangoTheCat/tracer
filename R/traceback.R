
#' A better `base::traceback()`
#'
#' Compared to `base::traceback()`, `tb`
#' * shows the stack from top to bottom, the same way as [utils::recover()],
#' * shows references to source files in a clickable form (in terminals
#'   that support this),
#' * cuts long lines of code,
#' * colors the output nicely (although this is a matter of taste),
#' * can show the source code where the error happened, with syntax
#'   highlighting, if the source code is available.
#'
#' `bt` is an alias to `tb`.
#'
#' @section The custom error handler:
#' `tracer` defines an error handler when loaded, via a call to
#' `options(error = tracer:::dumper)`. If you overwrite the error handler,
#' then `tb` does not work properly. You can reinstate the `tracer:::dumper`
#' error handler via an explicit call to `options()`.
#'
#' @param frame If `NULL`, the stack trace is shown. If it is an integer
#'   referring to a frame in the stack, the source code of the corresponding
#'   call is shown.
#' @param context Number of source code lines to show before and after
#'   the current line.
#'
#' @export

tb <- function(frame = NULL, context = 5) {
  if (is.null(frame)) {
    format_trace(data$last_dump)
  } else {
    if (!is_count(frame) || frame > length(data$last_dump$calls)) {
      message("\nInvalid frame: ", frame)
      return(invisible())
    }

    trace_code(data$last_dump, frame, context)
  }
}

#' @export
#' @rdname tb

bt <- tb
