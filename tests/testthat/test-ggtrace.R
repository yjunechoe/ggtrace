library(ggplot2)

global_ggtrace_state(TRUE)

boxplot_plot <- ggplot(iris, aes(Species, Sepal.Length)) + geom_boxplot()

test_that("trace is created (messages)", {

  expect_message(gguntrace(Stat$compute_layer), "not currently being traced")

  expect_message(
    ggtrace(
      Stat$compute_layer,
      trace_steps = c(1, 3),
      trace_exprs = rlang::exprs(head(data), head(data)),
      verbose = FALSE
    ),
    "now being traced"
  )

  expect_message(gguntrace(Stat$compute_layer), "no longer being traced")
  expect_message(gguntrace(Stat$compute_layer), "not currently being traced")

})

test_that("trace is created, triggered, then removed", {

  expect_message(
    ggtrace(
      Stat$compute_layer,
      trace_steps = c(1, 3),
      trace_exprs = rlang::exprs(head(data), head(data)),
      verbose = FALSE
    ),
    "now being traced"
  )
  invisible(ggplotGrob(boxplot_plot))
  expect_message(gguntrace(Stat$compute_layer), "not currently being traced")

})

test_that("trace will get overriden", {

  expect_message(
    ggtrace(
      Stat$compute_layer,
      trace_steps = c(1, 3),
      trace_exprs = rlang::exprs(head(data), head(data)),
      verbose = FALSE
    ),
    "now being traced"
  )
  invisible(ggplotGrob(boxplot_plot))
  expect_message(gguntrace(Stat$compute_layer), "not currently being traced")
  as_is <- last_ggtrace()

  expect_message(
    ggtrace(
      Stat$compute_layer,
      trace_steps = c(1, 3),
      trace_exprs = rlang::exprs(head(data), head(data)),
      verbose = FALSE
    ),
    "now being traced"
  )
  expect_message(
    ggtrace(
      Stat$compute_layer,
      trace_steps = c(1, 3),
      trace_exprs = rlang::exprs(self, self),
      verbose = FALSE
    ),
    "now being traced"
  )
  invisible(ggplotGrob(boxplot_plot))
  expect_message(gguntrace(Stat$compute_layer), "not currently being traced")
  overriden <- last_ggtrace()

  expect_true(!identical(as_is, overriden))

})

test_that("once = FALSE creates persistent trace", {

  expect_message(gguntrace(Stat$compute_layer), "not currently being traced")

  expect_message(
    ggtrace(
      Stat$compute_layer,
      trace_steps = c(1, 3),
      trace_exprs = rlang::exprs(head(data), head(data)),
      once = FALSE,
      verbose = FALSE
    ),
    "now being traced"
  )

  clear_global_ggtrace()
  invisible(ggplotGrob(boxplot_plot))
  first <- global_ggtrace()

  invisible(ggplotGrob(boxplot_plot))
  second <- global_ggtrace()
  clear_global_ggtrace()

  expect_equal(first, second[1])
  expect_equal(first[[1]], second[[2]])

  expect_message(gguntrace(Stat$compute_layer), "no longer being traced")
  expect_message(gguntrace(Stat$compute_layer), "not currently being traced")

})

test_that("trace_exprs length-1 exprs can be recycled", {

  expect_message(gguntrace(Stat$compute_layer), "not currently being traced")

  expect_message(
    ggtrace(
      Stat$compute_layer,
      trace_steps = c(1, 3),
      trace_exprs = rlang::exprs(head(data), head(data)),
      verbose = FALSE
    ),
    "now being traced"
  )
  invisible(ggplotGrob(boxplot_plot))
  expect_message(gguntrace(Stat$compute_layer), "not currently being traced")

  explicit <- last_ggtrace()
  expect_equal(names(explicit), c("", ""))

  expect_message(
    ggtrace(
      Stat$compute_layer,
      trace_steps = c(1, 3),
      trace_exprs = rlang::exprs(head(data)),
      verbose = FALSE
    ),
    "now being traced"
  )
  invisible(ggplotGrob(boxplot_plot))
  expect_message(gguntrace(Stat$compute_layer), "not currently being traced")

  implicit <- last_ggtrace()
  expect_equal(explicit, implicit)
  expect_equal(names(implicit), c("", ""))

})

test_that("trace_exprs single expr can be recycled", {

  expect_message(
    ggtrace(
      Stat$compute_layer,
      trace_steps = c(1, 3),
      trace_exprs = list(quote(head(data)), quote(head(data))),
      verbose = FALSE
    ),
    "now being traced"
  )
  invisible(ggplotGrob(boxplot_plot))
  expect_message(gguntrace(Stat$compute_layer), "not currently being traced")

  explicit <- last_ggtrace()

  expect_message(
    ggtrace(
      Stat$compute_layer,
      trace_steps = c(1, 3),
      trace_exprs = quote(head(data)),
      verbose = FALSE
    ),
    "now being traced"
  )
  invisible(ggplotGrob(boxplot_plot))
  expect_message(gguntrace(Stat$compute_layer), "not currently being traced")

  implicit <- last_ggtrace()

  expect_equal(explicit, implicit)
  expect_null(names(explicit))

})

test_that("trace_expr can take a list of exprs as value", {

  exprs_list1 <- rlang::exprs(head(data), head(data))
  expect_message(
    ggtrace(
      Stat$compute_layer,
      trace_steps = c(1, 3),
      trace_exprs = exprs_list1,
      verbose = FALSE
    ),
    "now being traced"
  )

  expect_message(gguntrace(Stat$compute_layer), "no longer being traced")

  exprs_list2 <- rlang::exprs(head(data))
  expect_message(
    ggtrace(
      Stat$compute_layer,
      trace_steps = c(1, 3),
      trace_exprs = exprs_list2,
      verbose = FALSE
    ),
    "now being traced"
  )

  expect_message(gguntrace(Stat$compute_layer), "no longer being traced")

})

test_that("clean_names = TRUE preserves names", {

  exprs_list1 <- rlang::exprs(first_head = head(data), second_head = head(data))
  expect_message(
    ggtrace(
      Stat$compute_layer,
      trace_steps = c(1, 3),
      trace_exprs = exprs_list1,
      verbose = FALSE
    ),
    "now being traced"
  )

  invisible(ggplotGrob(boxplot_plot))
  expect_equal(names(last_ggtrace()), names(exprs_list1))
  expect_message(gguntrace(Stat$compute_layer), "not currently being traced")

  exprs_list2 <- rlang::exprs(head = head(data), head(data))
  expect_message(
    ggtrace(
      Stat$compute_layer,
      trace_steps = c(1, 3),
      trace_exprs = exprs_list2,
      verbose = FALSE
    ),
    "now being traced"
  )

  invisible(ggplotGrob(boxplot_plot))
  expect_equal(names(last_ggtrace()), names(exprs_list2))
  expect_message(gguntrace(Stat$compute_layer), "not currently being traced")

})

test_that("NULL handled like any other value", {

  # Alone unnamed
  ggtrace(Stat$compute_layer, c(1), list(quote(NULL)), verbose = FALSE)
  invisible(ggplotGrob(boxplot_plot))
  null_alone_unnamed <- last_ggtrace()

  expect_true(is.list(null_alone_unnamed))
  expect_equal(length(null_alone_unnamed), 1)
  expect_null(names(null_alone_unnamed))
  expect_null(null_alone_unnamed[[1]])

  # Alone named
  ggtrace(Stat$compute_layer, c(1), list(null = quote(NULL)), verbose = FALSE)
  invisible(ggplotGrob(boxplot_plot))
  null_alone_named <- last_ggtrace()

  expect_true(is.list(null_alone_named))
  expect_equal(length(null_alone_named), 1)
  expect_equal(names(null_alone_named), "null")
  expect_null(null_alone_named[[1]])

  # Last
  ggtrace(Stat$compute_layer, c(1, 1), list(hi = quote(1), bye = quote(NULL)), verbose = FALSE)
  invisible(ggplotGrob(boxplot_plot))
  null_last <- last_ggtrace()

  expect_true(is.list(null_last))
  expect_equal(length(null_last), 2)
  expect_equal(names(null_last), c("hi", "bye"))
  expect_equal(vapply(null_last, is.null, logical(1), USE.NAMES = FALSE), c(FALSE, TRUE))

  # Middle
  ggtrace(
    Stat$compute_layer, c(1, 2, 3),
    list(hi = quote(1), bye = quote(NULL), byebye = quote(2)),
    verbose = FALSE
  )
  invisible(ggplotGrob(boxplot_plot))
  null_middle <- last_ggtrace()

  expect_true(is.list(null_middle))
  expect_equal(length(null_middle), 3)
  expect_equal(names(null_middle), c("hi", "bye", "byebye"))
  expect_equal(vapply(null_middle, is.null, logical(1), USE.NAMES = FALSE), c(FALSE, TRUE, FALSE))

})

test_that("incomplete traces are logged appropriately", {

  gguntrace(ggplot2:::Layer$map_statistic)
  clear_global_ggtrace()
  ggtrace(
    ggplot2:::Layer$map_statistic,
    seq_len(length(ggbody(ggplot2:::Layer$map_statistic))),
    quote(1 + 1),
    verbose = FALSE
  )
  expect_warning(invisible(ggplotGrob(ggplot())), "incomplete")
  expect_equal(length(last_ggtrace()), 2)
  expect_equal(length(unique(last_ggtrace())), 1)
  expect_true(grepl("INCOMPLETE", names(global_ggtrace())))

  clear_global_ggtrace()
  ggtrace(
    ggplot2:::Layer$map_statistic,
    seq_len(length(ggbody(ggplot2:::Layer$map_statistic))),
    quote(1 + 1),
    verbose = FALSE
  )
  expect_warning(invisible(ggplotGrob(ggplot(mtcars, aes(mpg, hp)) + geom_point())), "incomplete")
  expect_equal(length(last_ggtrace()), 8)
  expect_equal(length(unique(last_ggtrace())), 1)
  expect_true(grepl("INCOMPLETE", names(global_ggtrace())))

  clear_global_ggtrace()
  ggtrace(
    ggplot2:::Layer$map_statistic,
    seq_len(length(ggbody(ggplot2:::Layer$map_statistic))),
    quote(1 + 1),
    once = FALSE,
    verbose = FALSE
  )
  expect_warning({
    invisible(ggplotGrob({
      ggplot(mtcars) +
        geom_point(aes(mpg, hp)) +
        geom_point(aes(mpg, stage(hp, y)))
    }))
  }, "incomplete")
  gguntrace(ggplot2:::Layer$map_statistic)
  partial_incomplete <- global_ggtrace()
  expect_true(grepl("INCOMPLETE", names(partial_incomplete[1])))
  expect_equal(length(partial_incomplete[[1]]), 8)
  expect_true(isFALSE(grepl("INCOMPLETE", names(partial_incomplete[2]))))
  expect_equal(length(partial_incomplete[[2]]), 21)

  ggtrace(ggplot2:::Layer$map_statistic, 10, quote(1 + 1), verbose = FALSE)
  expect_warning(invisible(ggplotGrob(ggplot())), "incomplete")
  expect_null(last_ggtrace())

  ggtrace(ggplot2:::Layer$map_statistic, c(1, 2, 10), quote(1 + 1), verbose = FALSE)
  expect_warning(invisible(ggplotGrob(ggplot())), "incomplete")
  expect_true(length(last_ggtrace()) == 2)

})

global_ggtrace_state(FALSE)
