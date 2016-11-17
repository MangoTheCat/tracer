
data <- new.env(parent = emptyenv())

#' @importFrom utils getSrcDirectory getSrcFilename getSrcLocation

dumper <- function() {

  calls <- drop_last(sys.calls())
  funcs <- lapply(seq_along(calls), sys.function)
  msg <- geterrmessage()

  tryCatch(
    dumper2(calls, funcs, msg),
    error = function(e) e
  )
  invisible()
}

dumper2 <- function(calls, funcs, msg) {

  dump <- list(
    nums  = format(seq_along(calls)),
    calls = calls,
    funcs = funcs,
    envs  = vapply(funcs, function(x) environmentName(environment(x)), ""),
    fnams = vapply(calls, get_call_name, ""),
    fargs = vapply(calls, get_call_args, ""),
    dirs  = nullna(lapply(calls, getSrcDirectory)),
    files = nullna(lapply(calls, getSrcFilename)),
    lines = nullna(lapply(calls, getSrcLocation)),
    cols  = nullna(lapply(calls, getSrcLocation, which = "column")),
    error = msg
  )

  data$last_dump <- dump
}

get_call_name <- function(call) {
  if (is.call(call) && is.symbol(call[[1]])) {
    as.character(call[[1]])
  } else {
    "<anonymous>"
  }
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
