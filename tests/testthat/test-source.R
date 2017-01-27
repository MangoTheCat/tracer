
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
  exp <- "\n  \033[34m  8\033[39m     crayon.enabled = \033[34mTRUE\033[39m\n  \033[34m  9\033[39m   )\n  \033[34m 10\033[39m   \033[36mon.exit\033[39m(\033[36moptions\033[39m(restore_width))\n  \033[34m 11\033[39m \n  \033[34m 12\033[39m   dump \033[32m<-\033[39m \033[34m\033[1mNULL\033[22m\033[39m\n\033[41m\033[37m ❯ 13 \033[39m\033[49m  f \033[32m<-\033[39m \033[31mfunction\033[39m(x) \033[36mg\033[39m()                  \033[38;5;247m\033[3m# do not change these lines\033[23m\033[39m\n  \033[34m 14\033[39m   g \033[32m<-\033[39m \033[31mfunction\033[39m(y) \033[36mh\033[39m(foo)\n  \033[34m 15\033[39m   h \033[32m<-\033[39m \033[31mfunction\033[39m(z) dump \033[32m<<-\033[39m \033[36mdumper\033[39m()\n  \033[34m 16\033[39m \n  \033[34m 17\033[39m   \033[36mf\033[39m()\n  \033[34m 18\033[39m \n\n   \033[33mf\033[39m\033[32m at "

  expect_match(out, exp, fixed = TRUE)

  out <- get_output(trace_code_with_source(dump, no_frames - 1, 5))
  exp <- "\n  \033[34m  8\033[39m     crayon.enabled = \033[34mTRUE\033[39m\n  \033[34m  9\033[39m   )\n  \033[34m 10\033[39m   \033[36mon.exit\033[39m(\033[36moptions\033[39m(restore_width))\n  \033[34m 11\033[39m \n  \033[34m 12\033[39m   dump \033[32m<-\033[39m \033[34m\033[1mNULL\033[22m\033[39m\n\033[41m\033[37m ❯ 13 \033[39m\033[49m  f \033[32m<-\033[39m \033[31mfunction\033[39m(x) \033[36mg\033[39m()                  \033[38;5;247m\033[3m# do not change these lines\033[23m\033[39m\n  \033[34m 14\033[39m   g \033[32m<-\033[39m \033[31mfunction\033[39m(y) \033[36mh\033[39m(foo)\n  \033[34m 15\033[39m   h \033[32m<-\033[39m \033[31mfunction\033[39m(z) dump \033[32m<<-\033[39m \033[36mdumper\033[39m()\n  \033[34m 16\033[39m \n  \033[34m 17\033[39m   \033[36mf\033[39m()\n  \033[34m 18\033[39m \n\n   \033[33mf\033[39m\033[32m at "
})

test_that("trace_code, top level", {
  expect_message(trace_code(NULL, 1, 5), "Cannot show calls from top level")
})
