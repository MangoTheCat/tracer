
context("styles")

test_that("style_env", {
  expect_equal(style_env("", tracer_default_style()), "")
})
