
context("source")

test_that("trace_code with source, colors", {
  restore_width <- options(
    width = 80,
    tracer.style = tracer_default_style(),
    crayon.enabled = TRUE
  )
  on.exit(options(restore_width))

  dump <- NULL
  f <- function(x) g()                  # do not change these lines
  g <- function(y) h(foo)
  h <- function(z) dump <<- dumper()

  f()

  no_frames <- length(dump$calls)

  out <- get_output(trace_code(dump, no_frames - 1, 5))
  expect_true(crayon::has_style(out))

  exp <- win_newline("\n    8     crayon.enabled = TRUE\n    9   )\n   10   on.exit(options(restore_width))\n   11 \n   12   dump <- NULL\n > 13   f <- function(x) g()                  # do not change these lines\n   14   g <- function(y) h(foo)\n   15   h <- function(z) dump <<- dumper()\n   16 \n   17   f()\n   18 \n\n   f at")

  out2 <- crayon::strip_style(out)
  out2 <- gsub("❯", ">", out2)
  
  expect_match(out2, exp, fixed = TRUE)

  out <- get_output(trace_code_with_source(dump, no_frames - 1, 5))
  expect_true(crayon::has_style(out))

  exp <- win_newline("\n    8     crayon.enabled = TRUE\n    9   )\n   10   on.exit(options(restore_width))\n   11 \n   12   dump <- NULL\n > 13   f <- function(x) g()                  # do not change these lines\n   14   g <- function(y) h(foo)\n   15   h <- function(z) dump <<- dumper()\n   16 \n   17   f()\n   18 \n\n   f at")

  out2 <- crayon::strip_style(out)
  out2 <- gsub("❯", ">", out2)

  expect_match(out2, exp, fixed = TRUE)
})

test_that("trace_code, top level", {
  expect_message(trace_code(NULL, 1, 5), "Cannot show calls from top level")
})
