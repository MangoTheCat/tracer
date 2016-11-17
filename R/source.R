
#' @importFrom crayon bgRed
#' @importFrom prettycode highlight
#' @importFrom utils getSrcref

trace_code <- function(data, frame, context = 5) {

  src <- getSrcref(data$calls[[frame]])
  if (is.null(src)) {
    message("Cannot show code for this call. :(")
    return(invisible())
  }

  file <- file.path(data$dirs[frame], data$files[frame])
  line <- data$lines[frame]
  col  <- data$cols [frame]
  if (! is_readable_file(file) && is_count(line)) {
    message("Cannot access source code for this call. :(")
    return(invisible())
  }
  srclines <- readLines(file)
  hilines <- highlight(srclines)

  first <- max(line - context, 1)
  last  <- min(line + context, length(srclines))

  style <- getOption("tracer.style", tracer_default_style())

  cat("\n")
  cat(
    paste0(
      ifelse(
        first:last == line,
        bgRed("", symbol$pointer, format(first:last), ""),
        paste0("  ", style$num(format(first:last)), " ")
      ),
      hilines[first:last]
    ),
    sep = "\n"
  )
  cat("\n")
  cat(style$location(paste0("   at ", file, ":", line, ":", col)), "\n\n")

  invisible()
}
