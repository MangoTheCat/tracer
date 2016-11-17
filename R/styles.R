
tracer_default_style <- function() {

  list(
    num = num_style,
    env = env_style,
    arg = arg_style,
    fnam = fnam_style,
    error = error_style
  )
}

#' @importFrom crayon combine_styles white bold

error_style <- function(x) {
  bg <- make_style("firebrick", bg = TRUE)
  combine_styles(bg, white, bold)(trim_ws(x), "\n", sep = "")
}


#' @importFrom utils installed.packages

grey_envs <- memoise::memoise(function() {
  rownames(installed.packages(priority = "base"))
})


#' @importFrom crayon make_style blue

env_style <- function(x) {
  e <- paste0(
    ifelse(x %in% grey_envs(), make_style("grey50")(x), blue(x)),
    "::"
  )
  e [ x == "R_GlobalEnv" ] <- ""
  e
}


#' @importFrom crayon make_style

num_style <- function(x) {
  make_style("yellow")(format(x))
}


#' @importFrom crayon make_style

arg_style <- function(x) {
  darkgrey <- make_style("grey100")
  darkgrey(x)
}


#' @importFrom crayon make_style

fnam_style <- function(x) {
  combine_styles(make_style("gold2"), bold)(x)
}
