
#' @export

tracer <- function() {

  if (.isMethodsDispatchOn()) {
    ## turn off tracing
    tState <- tracingState(FALSE)
    on.exit(tracingState(tState))
  }

  if (identical(getOption("show.error.messages"), FALSE)) {
    ## from try(silent=TRUE)?
    return(NULL)
  }

  calls <- drop_last(sys.calls())
  funcs <- lapply(seq_along(calls), sys.function)

  data <- list(
    nums  = format(seq_along(calls)),
    calls = calls,
    funcs = funcs,
    envs  = vapply(funcs, function(x) environmentName(environment(x)), ""),
    fnams = vapply(calls, function(x) as.character(x[[1]]), ""),
    fargs = vapply(calls, format_call_args, ""),
    files = lapply(funcs, getSrcFilename),
    lines = lapply(funcs, getSrcLocation)
  )

  format_trace(data)
}


#' @importFrom clisymbols symbol

format_trace <- function(data) {

  msg <- get_error_message()

  cat("\n", error_style(msg), "\n", sep = "")

  if (length(data$calls) == 0) return()

  arg_cols <- (
    getOption("width", 80)
    - max(nchar(data$nums)) - 1
    - max(nchar(data$envs) + 2 + nchar(data$fnams)) - 1
  )

  args <- substring(data$fargs, 1, arg_cols)

  col_nums  <- num_style(data$nums)
  col_envs  <- env_style(data$envs)
  col_args  <- arg_style(args)
  col_fnams <- fnam_style(data$fnams)

  str_calls <- paste0(col_nums, " ", col_envs, "::", col_fnams, " ", col_args)

  cat(str_calls, sep = "\n")
  cat("\n")
}


format_call_args <- function(call) {
  args <- as.character(call[-1])
  args <- paste(args, collapse = ", ")
  args <- gsub("\\s+", " ", args)
  paste0("(", args, ")")
}


get_error_message <- function() {

  msg <- geterrmessage()
  msg <- trim_ws(msg)
  msg <- paste0(" ", symbol$cross, " ", msg)
  msg <- gsub("\n", " \n ", msg)
  msg <- paste0(msg, " \n")
  msg
}
