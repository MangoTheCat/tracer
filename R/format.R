
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
      - max(nchar(data$nums)) - 1
      - nchar(envs) - (nchar(envs) > 0) * 2
      - nchar(data$fnams) - 1
    )

    args <- ifelse(
      nchar(data$fargs) <= arg_cols,
      data$fargs,
      paste0(
        substring(data$fargs, 1, arg_cols - nchar(symbol$ellipsis, "width")),
        cyan(symbol$ellipsis)
      )
    )

    col_nums  <- style$num(data$nums)
    col_envs  <- vapply(envs, FUN.VALUE = "", style_env, style = style)
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

style_env <- function(e, style) {
  if (e %in% grey_envs()) {
    paste0(style$baseenv(e), "::")
  } else if (e != "") {
    paste0(style$env(e), "::")
  } else {
    ""
  }
}
