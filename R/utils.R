
trim_ws <- function(x) {
  sub("\\s+$", "", sub("^\\s+", "", x))
}

drop_last <- function(x) {
  head(x, n = length(x) - 1L)
}

`%+%` <- function(x, y) {
  paste0(x, y)
}

terminal_width <- function() {
  as.numeric(system("tput cols", intern = TRUE))
}

make_spaces <- function(n) {
  paste(rep(" ", n), collapse = "")
}

nullna <- function(x, unlist = TRUE) {
  x <- lapply(x, function(xx) if (is.null(xx) || ! length(xx)) NA else xx)
  if (unlist) unlist(x) else x
}
