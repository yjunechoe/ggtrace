library(ggplot2)

test_that("loops through `trace_exprs` correctly with persistent trace", {

  clear_global_ggtrace()

  ggtrace(
    GeomBoxplot$draw_group,
    trace_steps = c(1, 1, -1),
    trace_exprs = rlang::exprs(
      cond_check1 = {
        if (data$group %% 2 == 0) {
          TRUE
        } else {
          FALSE
        }
      },
      cond_check2 = data$group %% 2 == 0,
      data = data,
    ),
    once = FALSE,
    verbose = FALSE
  )

  boxplot <- ggplot(mpg, aes(class, hwy)) + geom_boxplot()
  print(boxplot)

  boxplot_tracedump <- global_ggtrace()

  # Condition met at expected places
  expect_equal(
    vapply(boxplot_tracedump, function(x) x$cond_check1, logical(1), USE.NAMES = FALSE),
    seq_len(length(unique(mpg$class))) %% 2 == 0
  )
  expect_equal(
    vapply(boxplot_tracedump, function(x) x$cond_check1, logical(1), USE.NAMES = FALSE),
    vapply(boxplot_tracedump, function(x) x$cond_check2, logical(1), USE.NAMES = FALSE)
  )

  expect_message(
    gguntrace(GeomBoxplot$draw_group),
    "no longer being traced"
  )
  expect_message(
    gguntrace(GeomBoxplot$draw_group),
    "not currently being traced"
  )

})

