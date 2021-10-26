clear_global_ggtrace()

test_that("tracing correctly identified for functions", {
  library(ggplot2)
  expect_message(gguntrace(ggplot), "not currently being traced")
  expect_true(isFALSE(is_traced(ggplot)))
  expect_message(ggtrace(ggplot, 1), "now being traced")
  expect_true(is_traced(ggplot))
  expect_message(gguntrace(ggplot), "no longer being traced")
  expect_true(isFALSE(is_traced(ggplot)))
  expect_message(gguntrace(ggplot), "not currently being traced")
})

test_that("tracing correctly identified for functions with ::", {
  expect_message(gguntrace(ggplot2::ggplot), "not currently being traced")
  expect_true(isFALSE(is_traced(ggplot2::ggplot)))
  expect_message(ggtrace(ggplot2::ggplot, 1), "now being traced")
  expect_true(is_traced(ggplot2::ggplot))
  expect_message(gguntrace(ggplot2::ggplot), "no longer being traced")
  expect_true(isFALSE(is_traced(ggplot2::ggplot)))
  expect_message(gguntrace(ggplot2::ggplot), "not currently being traced")
})

test_that("basic tracing tests for for custom functions", {
  test_fn <- function() {
    a <- 1
    b <- 2
    c <- 3
    result <- a + b + c
    result
  }
  expect_message(gguntrace(test_fn), "not currently being traced")
  expect_true(isFALSE(is_traced(test_fn)))
  expect_equal(length(as.list(body(test_fn))), 6)
  expect_equal(test_fn(), 6)

  ggtrace:::set_last_ggtrace(NULL)
  expect_null(last_ggtrace())
  expect_message(ggtrace(test_fn, 2:4, verbose = FALSE), "now being traced")
  invisible(test_fn())
  expect_true(isFALSE(is_traced(test_fn)))
  expect_equal(unlist(last_ggtrace()), 1:3)

  expect_message(ggtrace(test_fn, 3, quote(a <- 10), verbose = FALSE), "now being traced")
  result_modified <- invisible(test_fn())
  expect_equal(result_modified, 15)
  expect_true(result_modified != test_fn())

  expect_message(ggtrace(test_fn, -1, quote(result <- "hi"), verbose = FALSE), "now being traced")
  result_highjacked <- invisible(test_fn())
  expect_equal(result_highjacked, "hi")
  expect_true(result_highjacked != test_fn())

  expect_message(ggtrace(test_fn, -1, quote(result <- "persisting"), once = FALSE, verbose = FALSE), "persistent trace")
  expect_true(is_traced(test_fn))
  result_persistent1 <- test_fn()
  result_persistent2 <- test_fn()
  expect_equal(result_persistent1, "persisting")
  expect_equal(result_persistent1, result_persistent2)
  expect_equal(test_fn(), test_fn())
  expect_message(gguntrace(test_fn), "no longer being traced")
  expect_true(isFALSE(is_traced(test_fn)))

  expect_message(ggtrace(test_fn, -1, quote(environment()), once = FALSE, verbose = FALSE), "persistent trace")
  expect_true(is_traced(test_fn))
  expect_equal(test_fn(), 6)
  env_persistent1 <- last_ggtrace()[[1]]
  expect_equal(test_fn(), 6)
  env_persistent2 <- last_ggtrace()[[1]]
  expect_true(rlang::is_environment(env_persistent1))
  expect_true(rlang::is_environment(env_persistent2))
  expect_true(isFALSE(identical(env_persistent1, env_persistent2)))
  expect_equal(result_persistent1, result_persistent2)
  expect_message(gguntrace(test_fn), "no longer being traced")
  expect_true(isFALSE(is_traced(test_fn)))

})
