
prev <- NULL

.onLoad <- function(libname, pkgname) {
  prev <<- getOption("error")
  options(error = dumper)
}

.onUnload <- function(libpath) {
  options(error = prev)
}                    

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
    fargs = vapply(calls, format_call_args, ""),
    files = lapply(funcs, getSrcFilename),
    lines = lapply(funcs, getSrcLocation),
    error = geterrmessage()
  )

  data$last_dump <- dump

  invisible()
}
