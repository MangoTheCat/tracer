
#' @importFrom clisymbols symbol
#' @importFrom crayon cyan

format_trace <- function(data, style = NULL) {

  if (is.null(style)) {
    style <- getOption("tracer.style", tracer_default_style())
  }

  if (length(data$calls) == 0) {
    cat("No traceback available.\n")

  } else {

    envs <- data$envs
    envs[envs == "R_GlobalEnv"] <- ""

    arg_cols <- (
      getOption("width", 80)
      - max(nchar(data$nums)) - 2
      - nchar(envs) - (nchar(envs) > 0) * 2
      - nchar(data$fnams) - 3
    )

    args <- ifelse(
      nchar(data$fargs) <= arg_cols,
      data$fargs,
      paste(
        substring(data$fargs, 1, arg_cols - nchar(symbol$ellipsis, "width")),
        cyan(symbol$ellipsis)
      )
    )

    locs <- paste0(
      "\n   at ", data$dirs, .Platform$file.sep,
      data$files, ":", data$lines, ":", data$cols
    )

    col_nums  <- style$num(data$nums)
    col_envs  <- vapply(envs, FUN.VALUE = "", style_env, style = style)
    col_args  <- style$arg(args)
    col_fnams <- style$fnam(data$fnams)
    col_locs  <- style$location(
      ifelse(is.na(data$files) | data$files == "", "", locs)
    )

    str_calls <- paste(
      paste0(col_nums, " ", col_envs, col_fnams, " ", col_args),
      col_locs
    )

    cat("", str_calls, sep = "\n")

  }

  if (!is.null(data$error)) {
    cat("\n", style$error(data$error), sep = "")
  }

  invisible()
}
