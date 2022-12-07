global_ggtrace_state(TRUE)
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

test_that("tracing correctly identified for ::: generics", {
  expect_message(gguntrace(ggplot2:::ggplot_build.ggplot), "not currently being traced")
  expect_true(isFALSE(is_traced(ggplot2:::ggplot_build.ggplot)))
  expect_message(ggtrace(ggplot2:::ggplot_build.ggplot, 1), "now being traced")
  expect_true(is_traced(ggplot2:::ggplot_build.ggplot))
  expect_message(gguntrace(ggplot2:::ggplot_build.ggplot), "no longer being traced")
  expect_true(isFALSE(is_traced(ggplot2:::ggplot_build.ggplot)))
  expect_message(gguntrace(ggplot2:::ggplot_build.ggplot), "not currently being traced")
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

  expect_null(clear_last_ggtrace())
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
  expect_equal(mget(ls(env_persistent1), env_persistent1), mget(ls(env_persistent2), env_persistent2))
  expect_message(gguntrace(test_fn), "no longer being traced")
  expect_true(isFALSE(is_traced(test_fn)))

})

test_that("ggbody supports functions", {
  expect_equal(ggbody(sample), as.list(body(sample)))
  expect_equal(ggbody(ggplot2::mean_se), as.list(body(ggplot2::mean_se)))
  expect_equal(ggbody(ggforce:::add_y_pos), as.list(body(ggforce:::add_y_pos)))
  expect_equal(ggbody(ggplot2:::ggplot_build.ggplot), as.list(body(ggplot2:::ggplot_build.ggplot)))

  expect_equal(length(ggbody(ggplot2::mean_se)), 5)
  ggtrace(ggplot2::mean_se, 2)
  expect_warning(ggbody(ggplot2::mean_se), "currently being traced")
  expect_warning(len_after2 <- length(ggbody(ggplot2::mean_se)), "currently being traced")
  expect_equal(len_after2, 3)
  gguntrace(ggplot2::mean_se)

  library(ggplot2)

  expect_equal(length(ggbody(mean_se)), 5)
  ggtrace(mean_se, 2)
  expect_warning(ggbody(mean_se), "currently being traced")
  expect_warning(len_after1 <- length(ggbody(mean_se)), "currently being traced")
  expect_equal(len_after1, 3)
  gguntrace(mean_se)

  expect_message(ggtrace(mean_se, 2, once = FALSE), "persistent trace")
  expect_warning(ggbody(mean_se), "currently being traced")
  expect_true(is_traced(mean_se))

  # Odd behavior I might want to account for later (pt.1)
  expect_true(!"functionWithTrace" %in% class(get("mean_se")))
  expect_true("functionWithTrace" %in% class(get("mean_se", envir = asNamespace("ggplot2"))))

  expect_message(gguntrace(mean_se), "no longer being traced")
  expect_message(gguntrace(mean_se), "not currently being traced")
  expect_equal(ggbody(mean_se), as.list(body(mean_se)))

  # Odd behavior I might want to account for later (pt.2)
  identical(get("mean_se"), get("mean_se", envir = asNamespace("ggplot2")))

})

global_ggtrace_state(FALSE)
