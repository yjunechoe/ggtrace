library(ggplot2)

boxplot_plot <- ggplot(iris, aes(Species, Sepal.Length)) + geom_boxplot()


test_that("trace is created (messages)", {

  expect_message(gguntrace(Stat$compute_layer), "not currently being traced")

  expect_message(
    ggtrace(
      Stat$compute_layer,
      trace_steps = c(1, 3),
      trace_exprs = rlang::exprs(head(data), head(data))
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
      trace_exprs = rlang::exprs(head(data), head(data))
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
      trace_exprs = rlang::exprs(head(data), head(data))
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
      trace_exprs = rlang::exprs(head(data), head(data))
    ),
    "now being traced"
  )
  expect_message(
    ggtrace(
      Stat$compute_layer,
      trace_steps = c(1, 3),
      trace_exprs = rlang::exprs(self, self)
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
      once = FALSE
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
      trace_exprs = rlang::exprs(head(data), head(data))
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
      trace_exprs = rlang::exprs(head(data))
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
      trace_exprs = list(quote(head(data)), quote(head(data)))
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
      trace_exprs = quote(head(data))
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
      trace_exprs = exprs_list1
    ),
    "now being traced"
  )

  expect_message(gguntrace(Stat$compute_layer), "no longer being traced")

  exprs_list2 <- rlang::exprs(head(data))
  expect_message(
    ggtrace(
      Stat$compute_layer,
      trace_steps = c(1, 3),
      trace_exprs = exprs_list2
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
      trace_exprs = exprs_list1
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
      trace_exprs = exprs_list2
    ),
    "now being traced"
  )

  invisible(ggplotGrob(boxplot_plot))
  expect_equal(names(last_ggtrace()), names(exprs_list2))
  expect_message(gguntrace(Stat$compute_layer), "not currently being traced")

})
