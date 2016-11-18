
#' @importFrom crayon make_style combine_styles bold white red blue bgRed cyan yellow green

tracer_default_style <- function() {

  list(
    num = function(x) blue(paste0(" ", format(x))),
    baseenv = combine_styles(cyan, bold),
    env = cyan,
    arg = identity,
    fnam = yellow,
    error = error_style,
    location = green,
    pointer = combine_styles(bgRed, white)
  )
}

error_style <- function(x) {
  txt <- paste0(" ", symbol$cross, " ", trim_ws(x), "\n")
  combine_styles(bold, red)(txt)
}

#' @importFrom utils installed.packages

grey_envs <- c(
  rownames(installed.packages(priority = "base")),
  "R_GlobalEnv"
)

style_env <- function(e, style) {
  if (e %in% grey_envs) {
    paste0(style$baseenv(e), "::")
  } else if (e != "") {
    paste0(style$env(e), "::")
  } else {
    ""
  }
}
