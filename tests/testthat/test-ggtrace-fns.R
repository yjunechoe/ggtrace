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

test_that("Inspect returns same whether from build or Layer", {
  # Real world examples
  library(ggplot2)

  # Bar plot using computed/"mapped" aesthetics with `after_stat()` and `after_scale()`
  barplot_plot <- ggplot(data = palmerpenguins::penguins) +
    geom_bar(
      mapping = aes(
        x = species,                           # Discrete x-axis representing species
        y = after_stat(count / sum(count)),    # Bars represent count of species as proportions
        color = species,                       # The outline of the bars are colored by species
        fill = after_scale(alpha(color, 0.5))  # The fill of the bars are lighter than the outline color
      ),
      size = 3
    )

  clear_global_ggtrace()

  ggtrace(method = ggplot2:::Layer$map_statistic, trace_steps = -1, verbose = FALSE)
  barplot_plot
  inside <- last_ggtrace()[[1]]

  expect_equal(length(as.list(body(ggplot2:::ggplot_build.ggplot))), 33)
  ggtrace(ggplot2:::ggplot_build.ggplot, 19, verbose = FALSE)
  expect_equal(length(as.list(body(ggplot2:::ggplot_build.ggplot))), 3)
  barplot_plot
  outside <- last_ggtrace()[[1]]

  combined <- global_ggtrace()

  expect_equal(inside, combined[[1]][[1]])
  expect_equal(outside, combined[[2]][[1]])
  expect_equal(inside, outside[[1]])
  expect_true(grepl("^ggplot2:::Layer\\$map_statistic", names(combined)[1]))
  expect_true(grepl("^ggplot2:::ggplot_build\\.ggplot", names(combined)[2]))
})

