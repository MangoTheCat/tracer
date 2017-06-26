
prev_env <- new.env(parent = emptyenv())

.onAttach <- function(libname, pkgname) {
  prev_env$prev <- getOption("error")   # nocov
  options(error = dumper)               # nocov
}

.onDetach <- function(libpath) {
  options(error = prev_env$prev)        # nocov
}
