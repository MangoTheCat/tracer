
prev <- NULL

.onLoad <- function(libname, pkgname) {
  prev <<- getOption("error")
  options(error = dumper)
}

.onUnload <- function(libpath) {
  options(error = prev)
}                    
