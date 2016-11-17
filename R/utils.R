
trim_ws <- function(x) {
  sub("\\s+$", "", sub("^\\s+", "", x))
}

#' @importFrom utils head

drop_last <- function(x, n) {
  head(x, n = length(x) - n)
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

na0 <- function(x) {
  ifelse(is.na(x), 0, x)
}

is_count <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x) && as.integer(x) == x
}

is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

## a file we can read from

is_readable_file <- function(x) {
  is_string(x) && file.exists(x) &&
    tryCatch(
      length(readLines(x, n = 1)) == 1,
      error = function(e) FALSE
    )
}
