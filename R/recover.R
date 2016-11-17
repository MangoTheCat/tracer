
#' @export

recover <- function() {

  ## TODO

  if (.isMethodsDispatchOn()) {
    ## turn off tracing
    tState <- tracingState(FALSE)
    on.exit(tracingState(tState))
  }

  ## Need to have a better test for try()
  ## if (identical(getOption("show.error.messages"), FALSE)) {
  ##   ## from try(silent=TRUE)?
  ##   return(NULL)
  ## }

}
