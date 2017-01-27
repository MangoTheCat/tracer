
context("traceback")

test_that("tb()", {

  mockery::stub(tb, "format_trace", function(x) "ok")
  expect_identical(tb(), "ok")
})

test_that("tb(frame)", {

  expect_message(ret <- tb(1000000), "Invalid frame")
  expect_null(ret)

  mockery::stub(tb, "trace_code", function(a,b,c) list(a,b,c))
  res <- tb(1, 10)
  expect_equal(res[[2]], 1)
  expect_equal(res[[3]], 10)
})
