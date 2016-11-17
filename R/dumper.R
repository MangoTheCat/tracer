
data <- new.env(parent = emptyenv())

dumper <- function() {
  calls <- drop_last(sys.calls())
  funcs <- lapply(seq_along(calls), sys.function)
  
  dump <- list(
    nums  = format(seq_along(calls)),
    calls = calls,
    funcs = funcs,
    envs  = vapply(funcs, function(x) environmentName(environment(x)), ""),
    fnams = vapply(calls, function(x) as.character(x[[1]]), ""),
    fargs = vapply(calls, get_call_args, ""),
    files = lapply(funcs, getSrcFilename),
    lines = lapply(funcs, getSrcLocation),
    cols  = lapply(funcs, getSrcLocation, which = "column"),
    error = geterrmessage()
  )

  data$last_dump <- dump

  invisible()
}

get_call_args <- function(call) {
  ## No arguments
  if (is.null(call[-1])) return("()")

  ## Otherwise format them
  call[[1]] <- as.symbol("foobar")
  str <- format(call)
  str[1] <- sub("^foobar", "", str[1])
  if (length(str) > 1) {
    str[-1] <- sub("^[ ]+", "", str[-1])
  }
  paste(str, collapse = "")
}
