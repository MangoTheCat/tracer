
context("utils")

test_that("trim_ws", {
  cases <- list(
    list("", ""),
    list("a", "a"),
    list(letters, letters),
    list(paste("", letters), letters),
    list(paste(letters, ""), letters),
    list(paste("", letters, ""), letters),
    list(paste("", ""), ""),
    list(paste(character(10), ""), character(10))
  )

  for (x in seq_along(cases)) {
    expect_identical(trim_ws(cases[[x]][[1]]), cases[[x]][[2]], info = x)
  }
})

test_that("drop_last", {
  cases <- list(
    list(1:5, 1, 1:4),
    list(1:5, 2, 1:3),
    list(1:5, 5, integer()),
    list(1, 0, 1),
    list(numeric(), 0, numeric()),
    list(numeric(), 1, numeric()),

    list(list(1,2,3), 1, list(1,2)),
    list(list(1,2,3), 3, list()),
    list(list(1,2,3), 0, list(1,2,3))
  )

  for (x in seq_along(cases)) {
    expect_identical(
      drop_last(cases[[x]][[1]], cases[[x]][[2]]),
      cases[[x]][[3]],
      info = x
    )
  }
})

test_that("make_spaces", {
  cases <- list(
    list(1, " "),
    list(3, "   "),
    list(0, "")
  )

  for (x in seq_along(cases)) {
    expect_identical(make_spaces(cases[[x]][[1]]), cases[[x]][[2]], info = x)
  }
})

test_that("nullna", {
  cases <- list(
    list(list(1,2,3), list(1,2,3)),
    list(list(1, NULL, 2), list(1, NA, 2)),
    list(list(NULL), list(NA)),
    list(list(NULL, NULL), list(NA, NA))
  )

  for (x in seq_along(cases)) {
    expect_identical(nullna(cases[[x]][[1]], FALSE), cases[[x]][[2]], info = x)
  }
  for (x in seq_along(cases)) {
    expect_identical(nullna(cases[[x]][[1]]), unlist(cases[[x]][[2]]), info = x)
  }
})

test_that("na0", {
  cases <- list(
    list(numeric(), numeric()),
    list(1, 1),
    list(NA_real_, 0),
    list(c(NA_real_, NA_real_), c(0, 0)),
    list(c(1:5, NA_real_, 6:10), c(1:5, 0, 6:10)),
    list(c(1:5, NA_integer_, 6:10), c(1:5, 0L, 6:10))
  )

  for (x in seq_along(cases)) {
    expect_identical(na0(cases[[x]][[1]]), cases[[x]][[2]], info = x)
  }
})

test_that("is_count", {
  pos <- list(1, 1L, 0, 0L)
  neg <- list("1", 1.1, -1L, numeric(), NA, NA_integer_, NA_real_)
  for (x in pos) expect_true(is_count(x), info = x)
  for (x in neg) expect_false(is_count(x), info = x)
})

test_that("is_string", {
  pos <- list("blah", letters[1], "")
  neg <- list(1, 1:5, character(), letters, c(NA, ""), NA_character_)
  for (x in pos) expect_true(is_string(x), info = x)
  for (x in neg) expect_false(is_string(x), info = x)
})

test_that("is_readable_file", {
  cat("boo\n", file = tmp <- tempfile())
  expect_true(is_readable_file(tmp))
  expect_false(is_readable_file(tempfile()))
})
