
context("source 2")

test_that("trace_code without source, no colors", {
  restore_width <- options(
    width = 80,
    tracer.style = tracer_default_style(),
    crayon.enabled = FALSE
  )
  on.exit(options(restore_width))

  dump <- NULL
  f <- function(x) g()                  # do not change these lines
  g <- function(y) h(foo)
  h <- function(z) dump <<- dumper()

  f()

  no_frames <- length(dump$calls)
  attr(dump$calls[[no_frames - 1]], "srcref") <- NULL

  out <- get_output(trace_code(dump, no_frames - 1, 5))
  exp <- win_newline("\n   1 function (x) \n > 2 g()\n\n   f (?), function without source\n\n")

  out2 <- gsub("\u276F", ">", out)
  expect_match(out2, exp, fixed = TRUE)
})
