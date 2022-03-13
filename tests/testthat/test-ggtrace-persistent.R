library(ggplot2)

global_ggtrace_state(TRUE)

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

  boxplot_plot <- ggplot(mpg, aes(class, hwy)) + geom_boxplot()
  invisible(ggplotGrob(boxplot_plot))

  boxplot_tracedump <- global_ggtrace()
  clear_global_ggtrace()

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

test_that("copy of traced function have same `once` behavior", {

  ellipse_plot <- ggplot(mtcars, aes(mpg, hp)) +
    stat_ellipse(
      aes(color = as.factor(cyl), fill = after_scale(alpha(color, 0.5))),
      geom = "polygon"
    ) +
    geom_point()

  ggtrace(GeomPolygon$draw_key, -1, verbose = FALSE)
  invisible(ggplotGrob(ellipse_plot))

  expect_equal(last_ggtrace()[[1]]$gp$fill, layer_data(ellipse_plot, 1)$fill[1])
  expect_true(!is_traced(GeomPolygon$draw_key))

  clear_global_ggtrace()

  ggtrace(GeomPolygon$draw_key, -1, verbose = FALSE, once = FALSE)
  invisible(ggplotGrob(ellipse_plot))
  gguntrace(GeomPolygon$draw_key)

  expect_equal(length(global_ggtrace()), nlevels(as.factor(mtcars$cyl)))
  expect_equal(rev(global_ggtrace())[[1]][[1]]$gp$fill, rev(layer_data(ellipse_plot, 1)$fill)[1])

})

global_ggtrace_state(FALSE)
