library(ggplot2)

test_that("inspection workflow works #1 (Position)", {

  ggtrace:::set_last_ggtrace(NULL)
  expect_null(last_ggtrace())

  jitter_plot <- ggplot(diamonds[1:1000,], aes(cut, depth)) +
    geom_point(position = position_jitter(width = 0.2, seed = 2021))

  ggtrace(
    method = PositionJitter$compute_layer,
    trace_steps = c(1, 1, 9, 12),
    trace_exprs = rlang::exprs(
      data,            # What does the data passed in look like?
      params,          # What do the initial parameters look like?
      dummy_data,      # What is `dummy_data` defined at Step 8?
      ~step            # What does the last line evaluate to?
      # - i.e., what is returned by the method?
    ),
    .print = FALSE     # Don't print evaluated expressions to console
  )

  expect_equal(
    last_ggtrace(),
    NULL
  )

  print(jitter_plot)

  jitter_tracedump <- last_ggtrace()
  expect_equal(
    jitter_tracedump[[2]],
    list(width = 0.2, height = 0.04, seed = 2021)
  )
  expect_equal(
    vapply(jitter_tracedump[-2], nrow, integer(1), USE.NAMES = FALSE),
    rep(1000L, 3)
  )

  gguntrace(PositionJitter$compute_layer)
})

test_that("inspection workflow works #2 (Geom)", {

  ggtrace:::set_last_ggtrace(NULL)
  expect_null(last_ggtrace())

  smooth_plot <- ggplot(mtcars, aes(mpg, hp)) +
    geom_point() +
    stat_smooth(method = "loess", formula = y ~ x)

  ggtrace(
    method = GeomSmooth$draw_group,
    trace_steps = -1,           # Trace the last line
    trace_exprs = quote(~step), # Grab the gList() object it returns
    .print = FALSE
  )

  print(smooth_plot)

  smooth_tracedump <- last_ggtrace()
  smooth_gList <- smooth_tracedump[[1]]

  expect_equal(
    as.numeric(smooth_gList[[1]]$children[[2]]$x),
    as.numeric(ggplotGrob(smooth_plot)[["grobs"]][[6]]$children[[4]]$children[[1]]$children[[2]]$x)
  )
  expect_equal(
    as.numeric(smooth_gList[[1]]$children[[1]]$x),
    as.numeric(ggplotGrob(smooth_plot)[["grobs"]][[6]]$children[[4]]$children[[1]]$children[[1]]$x)
  )

  gguntrace(GeomSmooth$draw_group)
})
