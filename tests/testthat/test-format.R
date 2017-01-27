
context("format")

test_that("format_trace", {
  restore_width <- options(width = 80, tracer.style = NULL,
                           crayon.enabled = FALSE)
  on.exit(options(restore_width))

  out <- get_output({
    format_trace(trace_data, style = tracer_default_style())
  })

  exp <- "\n 1 igraph::add_layout_ (nicely(), g) \n 2 base::`%in%` (\"layout\", graph_attr_names(graph)) \n 3 base::match (x, table, nomatch = 0L) \n 4 igraph::graph_attr_names (graph) \n 5 base::stop (\"Not a graph object\") \n\n ✖ Error in graph_attr_names(graph) : Not a graph object\n"

  expect_equal(exp, out)
})

test_that("format_trace with color", {
  restore_width <- options(width = 80, tracer.style = NULL,
                           crayon.enabled = TRUE)
  on.exit(options(restore_width))

  out <- get_output({
    format_trace(trace_data, style = tracer_default_style())
  })

  exp <- "\n\033[34m 1\033[39m \033[36migraph\033[39m::\033[33madd_layout_\033[39m (nicely(), g) \033[32m\033[39m\n\033[34m 2\033[39m \033[36m\033[1mbase\033[22m\033[39m::\033[33m`%in%`\033[39m (\"layout\", graph_attr_names(graph)) \033[32m\033[39m\n\033[34m 3\033[39m \033[36m\033[1mbase\033[22m\033[39m::\033[33mmatch\033[39m (x, table, nomatch = 0L) \033[32m\033[39m\n\033[34m 4\033[39m \033[36migraph\033[39m::\033[33mgraph_attr_names\033[39m (graph) \033[32m\033[39m\n\033[34m 5\033[39m \033[36m\033[1mbase\033[22m\033[39m::\033[33mstop\033[39m (\"Not a graph object\") \033[32m\033[39m\n\n\033[1m\033[31m ✖ Error in graph_attr_names(graph) : Not a graph object\n\033[39m\033[22m"

  expect_equal(exp, out)
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
