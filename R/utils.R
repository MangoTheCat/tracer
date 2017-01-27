
trim_ws <- function(x) {
  sub("\\s+$", "", sub("^\\s+", "", x))
}

## We need this, because head(x, -n) behaves badly if n = 0

#' @importFrom utils head

drop_last <- function(x, n) {
  head(x, n = length(x) - n)
}

make_spaces <- function(n) {
  paste(rep(" ", n), collapse = "")
}

nullna <- function(x, unlist = TRUE) {
  x <- lapply(x, function(xx) if (is.null(xx) || ! length(xx)) NA else xx)
  if (unlist) unlist(x) else x
}

## The special case is to avoid converting to logical():
## ifelse(numeric(), 0, 1) is logical()
## Also, we want to avoid ifelse() simplifying to integer,
## so if the input is not integer we put in a real 0 (instead of 0L).

na0 <- function(x) {
  if (!length(x)) {
    x

  } else if (is.integer(x)) {
    ifelse(is.na(x), 0L, x)

  } else {
    ifelse(is.na(x), 0, x)
  }
}

is_count <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x) && as.integer(x) == x &&
    x >= 0L
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
