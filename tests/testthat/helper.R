
get_output <- function(..., stream = stdout()) {

  if (identical(stream, stdout())) {
    type <- "output"
  } else if (identical(stream, stderr())) {
    type <- "message"
  }

  cleanup <- function() {
    unlink(tmp)
    sink(NULL, type = type)
  }

  tmp <- tempfile()
  on.exit(cleanup())

  sink(tmp, type = type)
  force(...)

  ## Windows has some strange readBin and sink interplay,
  ## so we need to remove the sink before reading the file
  sink(NULL, type = type)
  x <- readBin(tmp, raw(0), n = file.info(tmp)$size)
  unlink(tmp)

  ## No cleanup is needed any more
  on.exit(force(1))

  rawToChar(x)
}

win_newline <- function(..., collapse = NULL) {
  x <- paste0(...)
  if (.Platform$OS.type == "windows") {
    x <- gsub("\n", "\r\n", x, fixed = TRUE)
  }
  x
}

trace_data <- structure(list(
  nums = c("1", "2", "3", "4", "5"),
  calls = list(
    quote(add_layout_(nicely(), g)),
    quote("layout" %in% graph_attr_names(graph)),
    quote(match(x, table, nomatch = 0L)),
    quote(graph_attr_names(graph)),
    quote(stop("Not a graph object"))
  ),
  funcs = list(
    function (graph, ..., overwrite = TRUE) {
      if (overwrite && "layout" %in% graph_attr_names(graph)) {
        graph <- delete_graph_attr(graph, "layout")
      }
      graph$layout <- layout_(graph, ...)
      graph
    },
    function (x, table) match(x, table, nomatch = 0L) > 0L,
    function (x, table, nomatch = NA_integer_, incomparables = NULL)
      .Internal(match(x, table, nomatch, incomparables)),
    function (graph) {
      if (!is_igraph(graph)) {
        stop("Not a graph object")
      }
      res <- base::.Call("R_igraph_mybracket2_names", graph,
                         9L, 2L, PACKAGE = "igraph")
      if (is.null(res)) {
        res <- character()
      }
      res
    },
    function (..., call. = TRUE, domain = NULL) {
      args <- list(...)
      if (length(args) == 1L && inherits(args[[1L]], "condition")) {
        cond <- args[[1L]]
        if (nargs() > 1L)
          warning("additional arguments ignored in stop()")
        message <- conditionMessage(cond)
        call <- conditionCall(cond)
        .Internal(.signalCondition(cond, message, call))
        .Internal(.dfltStop(message, call))
      } else .Internal(stop(call., .makeMessage(..., domain = domain)))
    }
  ),
  envs = c("igraph", "base", "base", "igraph", "base"),
  fnams = c("add_layout_", "`%in%`", "match", "graph_attr_names","stop"),
  fargs = c("(nicely(), g)", "(\"layout\", graph_attr_names(graph))",
    "(x, table, nomatch = 0L)", "(graph)", "(\"Not a graph object\")"),
  dirs = c(NA, NA, NA, NA, NA),
  files = c(NA, NA, NA, NA, NA),
  lines = c(NA, NA, NA, NA, NA),
  cols = c(NA, NA, NA, NA, NA),
  error = "Error in graph_attr_names(graph) : Not a graph object\n"
))
