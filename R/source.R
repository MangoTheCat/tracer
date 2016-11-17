
#' @importFrom prettycode highlight
#' @importFrom utils getSrcref

trace_code <- function(data, frame, context) {

  if (frame == 1) {
    ## TODO: just show the top level call....
    message("Cannot show calls from top level")

  } else if (is.null(getSrcref(data$calls[[frame]]))) {
    trace_code_without_source(data, frame, context)

  } else {
    trace_code_with_source(data, frame, context)
  }
}

trace_code_with_source <- function(data, frame, context) {

  file <- file.path(data$dirs[frame], data$files[frame])
  line <- data$lines[frame]
  col  <- data$cols [frame]
  if (! is_readable_file(file) && is_count(line)) {
    message("Cannot access source code for this call. :(")
    return(invisible())
  }
  srclines <- readLines(file)

  style <- getOption("tracer.style", tracer_default_style())

  location <- paste0(
    style_env(data$envs[frame - 1], style = style),
    style$fnam(data$fnams[frame - 1]),
    style$location(paste0(" at ", file, ":", line, ":", col))
  )
  show_source(srclines, line, context, location)

  invisible()
}

show_source <- function(srclines, line, context, location) {
  hilines <- highlight(srclines)

  first <- max(line - context, 1)
  last  <- min(line + context, length(srclines))

  style <- getOption("tracer.style", tracer_default_style())

  cat("\n")
  cat(
    paste0(
      ifelse(
        first:last == line,
        style$pointer("", symbol$pointer, format(first:last), ""),
        paste0("  ", style$num(format(first:last)), " ")
      ),
      hilines[first:last]
    ),
    sep = "\n"
  )
  cat("\n")
  cat("   ", location, "\n\n", sep = "")

  invisible()
}

function_call_tokens <- c("SYMBOL_FUNCTION_CALL", "SPECIAL")

#' @importFrom utils getParseData getParseText

trace_code_without_source <- function(data, frame, context) {

  call <- data$calls[[frame]]
  if (!is.symbol(call[[1]])) {
    message("Could not find source code for this call")
    return(invisible())
  }

  pd_call <- getParseData(parse(text = deparse(call), keep.source = TRUE))

  func <- data$funcs[[frame - 1]]
  func_txt <- deparse(func)
  pd_func <- getParseData(parse(text = func_txt, keep.source = TRUE))

  ## Need grandparent of the function call to get full expression
  c_c <- pd_func$parent[pd_func$token %in% function_call_tokens &
                          pd_func$text == as.character(call[[1]])]
  cand <- pd_func$parent[match(c_c, pd_func$id)]

  c_text <- lapply(cand, getParseText, parseData = pd_func)
  c_p <- lapply(c_text, function(x) parse(text = x, keep.source = TRUE))
  c_pd <- lapply(c_p, getParseData)

  c_match <- vapply(c_pd, FUN.VALUE = TRUE, match_tree, pd_call)

  if (!any(c_match)) {
    message("Could not find source code for this call")
    return(invisible())
  } else if (sum(c_match) > 1) {
    message("Multiple possible call location in source code, showing all")
  }

  style <- getOption("tracer.style", tracer_default_style())

  for (m in which(c_match)) {
    line <- pd_func$line1[ match(cand[m], pd_func$id) ]
    loc <- paste0(
      style_env(data$envs[frame - 1], style = style),
      style$fnam(data$fnams[frame - 1]),
      style$location(paste0(" (?), function without source"))
    )
    show_source(func_txt, line, context, loc)
  }

  invisible()
}

match_tree <- function(t1, t2) {

  if (nrow(t1) != nrow(t2)) return(FALSE)

  ## Make sure they are ordered the same way
  t1 <- t1[order(t1$line1, t1$col1, t1$line2, t1$col2), ]
  t2 <- t2[order(t2$line1, t2$col1, t2$line2, t2$col2), ]

  ## Create a map between ids, and check if it is homomorhic for parents
  ## I.e. if two nodes have the same parent in t1, the corresponding
  ## nodes must have the same parents in t2
  ## Plus all text must match as well.
  idmap <- structure(t2$id, names = as.character(t1$id))
  all(na0(idmap[as.character(t1$parent)]) == t2$parent) &&
    all(t1$text == t2$text)
}
