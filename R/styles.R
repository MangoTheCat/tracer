
#' @importFrom crayon red

error_style <- function(x) {
  red(x)
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


#' @importFrom crayon green

fnam_style <- function(x) {
  green(x)
}
