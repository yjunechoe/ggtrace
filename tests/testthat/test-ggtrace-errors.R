library(ggplot2)

test_that("errors on steps and exprs mismatch", {

  expect_message(gguntrace(Stat$compute_panel), "not currently being traced")
  expect_error(
    ggtrace(
      Stat$compute_panel,
      trace_steps = c(1, 1, -1, -1), # four steps but 5 exprs
      trace_exprs = rlang::exprs(
        beginning = mget(ls()),
        env_start = environment(),
        env_deep_start = rlang::env_clone(environment()),
        env_end = environment(),
        env_deep_end = rlang::env_clone(environment())
      ),
      print_output = FALSE
    ),
    "Length mismatch"
  )
  expect_message(gguntrace(Stat$compute_panel), "not currently being traced")

})

test_that("errors on tracing a property", {
  expect_message(gguntrace(Stat$compute_panel), "not currently being traced")
  expect_error(ggtrace(Stat$retransform, 1), "Cannot trace a non-function")
  expect_message(gguntrace(Stat$compute_panel), "not currently being traced")
})

test_that("errors on steps out of range", {
  expect_message(gguntrace(Stat$compute_panel), "not currently being traced")
  expect_equal(length(ggbody(Stat$compute_layer)), 7)
  expect_error(ggtrace(Stat$compute_layer, 10), "out of range")
  expect_error(ggtrace(Stat$compute_layer, -10), "out of range")
  expect_error(ggtrace(Stat$compute_layer, 0), "out of range")
  expect_error(ggtrace(Stat$compute_layer, c(1:7, 8)), "out of range")
  expect_error(ggtrace(Stat$compute_layer, c(0:7)), "out of range")
  expect_message(gguntrace(Stat$compute_panel), "not currently being traced")
})

test_that("errors on steps out of order", {
  expect_message(gguntrace(Stat$compute_panel), "not currently being traced")
  expect_error(ggtrace(Stat$compute_layer, c(3, 2, 1)), "must be a sorted numeric vector")
  expect_error(ggtrace(Stat$compute_layer, c(-1, -2, -3)), "must be a sorted numeric vector")
  expect_error(ggtrace(Stat$compute_layer, c(5, -5)), "must be a sorted numeric vector")
  expect_message(gguntrace(Stat$compute_panel), "not currently being traced")
})

test_that("errors on duplicates", {
  expect_message(gguntrace(Stat$compute_panel), "not currently being traced")
  expect_error(ggtrace(Stat$compute_layer, c(1, 1)), "Duplicate")
  expect_error(ggtrace(Stat$compute_layer, c(1, 1), quote(data)), "Duplicate")
  expect_error(ggtrace(Stat$compute_layer, c(1, 1), list(quote(data))), "Duplicate")
  expect_error(ggtrace(Stat$compute_layer, c(-1, -1), list(quote(data))), "Duplicate")
  expect_message(gguntrace(Stat$compute_panel), "not currently being traced")

  expect_message(ggtrace(Stat$compute_layer, c(5, -1)), "now being traced")
  expect_message(ggtrace(Stat$compute_layer, c(5, -2)), "now being traced")
  expect_error(ggtrace(Stat$compute_layer, c(5, -3), list(quote(data))), "Duplicate")
  expect_message(gguntrace(Stat$compute_panel), "not currently being traced")
})
