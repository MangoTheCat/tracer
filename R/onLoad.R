
prev <- NULL

.onLoad <- function(libname, pkgname) {
  prev <<- getOption("error")           # nocov
  options(error = dumper)               # nocov
}

.onUnload <- function(libpath) {
  options(error = prev)                 # nocov
}                    
