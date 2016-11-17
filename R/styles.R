
#' @importFrom crayon make_style combine_styles bold white red blue

tracer_default_style <- function() {

  list(
    num = function(x) make_style("yellow")(format(x)),
    env = env_style,
    arg = make_style("grey90"),
    fnam = combine_styles(make_style("gold3"), bold),
    error = error_style
  )
}

error_style <- function(x) {
  fg <- make_style("firebrick2")
  combine_styles(bold, fg)(trim_ws(x), "\n", sep = "")
}

#' @importFrom utils installed.packages

grey_envs <- memoise::memoise(function() {
  rownames(installed.packages(priority = "base"))
})

env_style <- function(x) {
  e <- paste0(
    ifelse(x %in% grey_envs(), make_style("grey50")(x), blue(x)),
    "::"
  )
  e [ x == "R_GlobalEnv" ] <- ""
  e
}
