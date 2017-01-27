
context("utils")

test_that("trim_ws", {

})

test_that("drop_last", {

})

test_that("make_spaces", {

})

test_that("nullna", {

})

test_that("na0", {
  cases <- list(
    list(numeric(), numeric()),
    list(1, 1),
    list(NA_real_, 0),
    list(c(NA_real_, NA_real_), c(0, 0)),
    list(c(1:5, NA, 6:10), c(1:5, 0, 6:10)),
    list(c(1:5, NA_integer_, 6:10), c(1:5, 0L, 6:10))
  )

  for (x in seq_along(cases)) {
    expect_equal(na0(cases[[x]][[1]]), cases[[x]][[2]], info = x)
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

})
