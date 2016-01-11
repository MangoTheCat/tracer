
#' @importFrom crayon bold white bgRed

error_style <- function(x) {
  bold $ white $ bgRed(x)
}


#' @importFrom utils installed.packages

grey_envs <- memoise::memoise(function() {
  rownames(installed.packages(priority="base"))
})


#' @importFrom crayon make_style blue

env_style <- function(x) {

  ifelse(x %in% grey_envs(), make_style("darkgrey")(x), blue(x))
}


#' @importFrom crayon yellow

num_style <- function(x) {
  yellow(format(x))
}


#' @importFrom crayon make_style

arg_style <- function(x) {
  darkgrey <- make_style("darkgrey")
  darkgrey(x)
}


#' @importFrom crayon blue black

fnam_style <- function(x) {
  bold $ black(x)
}
