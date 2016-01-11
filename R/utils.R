
trim_ws <- function(x) {
  sub("\\s+$", "", sub("^\\s+", "", x))
}

drop_last <- function(x) {
  head(x, n = length(x) - 1L)
}

`%+%` <- function(x, y) {
  paste0(x, y)
}

str <- as.character
