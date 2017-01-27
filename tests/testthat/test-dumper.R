
context("dumper")

test_that("dumper", {

  dump <- NULL
  f <- function(x) g()                  # do not move these lines
  g <- function(y) h(foo)
  h <- function(z) dump <<- dumper()

  f()

  expect_equal(tail(dump$calls, 3), c(quote(f()), quote(g()), quote(h(foo))))
  expect_equal(tail(dump$funcs, 3), c(f, g, h))
  expect_equal(tail(dump$envs, 3), c("", "", ""))
  expect_equal(tail(dump$fnams, 3), c("f", "g", "h"))
  expect_equal(tail(dump$fargs, 3), c("()", "()", "(foo)"))
  expect_equal(tail(dump$dirs, 3), rep(getwd(), 3))
  expect_equal(tail(dump$files, 3), rep("test-dumper.R", 3))
  expect_equal(tail(dump$lines, 3), c(11, 7, 8))
  expect_equal(tail(dump$cols, 3), c(3, 8, 8))
})

test_that("get_call_name", {
  cases <- list(
    list(quote(f()), "f"),
    list(quote(long_function_name(arg1, arg2, ...)), "long_function_name"),
    list(quote((function() {})()), "(function() {\n})"),
    list(quote(blah), "<anonymous>")
  )

  for (x in seq_along(cases)) {
    expect_equal(get_call_name(cases[[x]][[1]]), cases[[x]][[2]])
  }
})

test_that("get_call_args", {
  cases <- list(
    list(quote(f()), "()"),
    list(quote(f(foo, bar, ...)), "(foo, bar, ...)")
  )

  for (x in seq_along(cases)) {
    expect_equal(get_call_args(cases[[x]][[1]]), cases[[x]][[2]])
  }
})
