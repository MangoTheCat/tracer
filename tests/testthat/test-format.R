
context("format")

test_that("format_trace", {
  restore_width <- options(width = 80, tracer.style = NULL,
                           crayon.enabled = FALSE)
  on.exit(options(restore_width))

  out <- get_output({
    format_trace(trace_data, style = tracer_default_style())
  })

  exp <- win_newline("\n 1 igraph::add_layout_ (nicely(), g) \n 2 base::`%in%` (\"layout\", graph_attr_names(graph)) \n 3 base::match (x, table, nomatch = 0L) \n 4 igraph::graph_attr_names (graph) \n 5 base::stop (\"Not a graph object\")")

  expect_match(out, exp, fixed = TRUE)
})

test_that("format_trace with color", {
  restore_width <- options(width = 80, tracer.style = NULL,
                           crayon.enabled = TRUE)
  on.exit(options(restore_width))

  out <- get_output({
    format_trace(trace_data, style = tracer_default_style())
  })
  expect_true(crayon::has_style(out))

  exp <- win_newline("\n 1 igraph::add_layout_ (nicely(), g) \n 2 base::`%in%` (\"layout\", graph_attr_names(graph)) \n 3 base::match (x, table, nomatch = 0L) \n 4 igraph::graph_attr_names (graph) \n 5 base::stop (\"Not a graph object\") \n\n x Error in graph_attr_names(graph) : Not a graph object\n")

  out2 <- crayon::strip_style(out)
  out2 <- gsub("\u2716", "x", out2)
  out2 <- gsub("<U+2716>", "x", out2, fixed = TRUE)

  expect_match(out2, exp, fixed = TRUE)
})

test_that("format_trace default", {
  restore_width <- options(width = 80,
                           tracer.style = tracer_default_style(),
                           crayon.enabled = FALSE)
  on.exit(options(restore_width))

  out1 <- get_output({
    format_trace(trace_data, style = tracer_default_style())
  })
  out2 <- get_output({
    format_trace(trace_data)
  })

  expect_equal(out1, out2)
})

test_that("format_trace without calls", {
  expect_output(
    format_trace(NULL),
    "No traceback available"
  )
})
