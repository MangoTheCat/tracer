
#' @importFrom clisymbols symbol

format_trace <- function(data, style = NULL) {

  if (is.null(style)) {
    style <- getOption("tracer.style", tracer_default_style())
  }

  if (length(data$calls) == 0) {
    cat("No traceback available.\n")

  } else {

    arg_cols <- (
      getOption("width", 80)
      - max(nchar(data$nums)) - 1
      - max(nchar(data$envs) + 2 + nchar(data$fnams)) - 1
    )

    args <- substring(data$fargs, 1, arg_cols)

    col_nums  <- style$num(data$nums)
    col_envs  <- style$env(data$envs)
    col_args  <- style$arg(args)
    col_fnams <- style$fnam(data$fnams)

    str_calls <- paste0(col_nums, " ", col_envs, col_fnams, " ", col_args)

    cat("", str_calls, sep = "\n")

  }

  if (!is.null(data$error)) {
    cat("\n", style$error(data$error), sep = "")
  }

  invisible()
}


format_call_args <- function(call) {
  args <- as.character(call[-1])
  args <- paste(args, collapse = ", ")
  args <- gsub("\\s+", " ", args)
  paste0("(", args, ")")
}
