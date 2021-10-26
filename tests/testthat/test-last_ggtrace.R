library(ggplot2)

violin_plot <- ggplot(iris, aes(Species, Petal.Width)) + geom_violin()

test_that("~step evaluates step", {

  clear_last_ggtrace()
  expect_null(last_ggtrace())

  gguntrace(ggplot2:::Layer$compute_statistic)
  ggtrace(
    ggplot2:::Layer$compute_statistic,
    trace_steps = c(4, 4),
    trace_exprs = list(
      quote(rlang::env_clone(environment())),
      quote(~step)
    ),
    verbose = FALSE
  )
  invisible(ggplotGrob(violin_plot))
  violin_tracedump <- last_ggtrace()

  violin_eval_manual <- eval(
    expr = ggbody(ggplot2:::Layer$compute_statistic)[[4]],
    envir = violin_tracedump[[1]]
  )

  expect_equal(violin_tracedump[[2]], violin_eval_manual)

})

test_that("exprs evaluated at different steps return different values", {

  gguntrace(StatYdensity$compute_group)
  ggtrace(
    StatYdensity$compute_group,
    trace_steps = c(7, -1),
    trace_exprs = quote(dens),
    verbose = FALSE
  )
  invisible(ggplotGrob(violin_plot))
  violin_tracedump <- last_ggtrace()

  expect_false(identical(violin_tracedump[[1]], violin_tracedump[[2]]))

})

test_that("global tracedump can be reset", {

  expect_null(clear_global_ggtrace())
  expect_null(global_ggtrace())

})

test_that("global tracedump collects until untrace", {

  clear_global_ggtrace()
  ggtrace(
    StatYdensity$compute_group,
    trace_steps = c(7, -1),
    trace_exprs = quote(dens),
    once = FALSE,
    verbose = FALSE
  )
  invisible(ggplotGrob(violin_plot))
  violin_tracedump <- global_ggtrace()

  expect_equal(violin_tracedump[[length(violin_tracedump)]], last_ggtrace())
  expect_equal(length(violin_tracedump), length(unique(iris$Species)))

  expect_false(any(duplicated(violin_tracedump)))
  expect_false(any(duplicated(names(violin_tracedump))))
  expect_false(any(duplicated(unlist(violin_tracedump, recursive = FALSE))))

  gguntrace(StatYdensity$compute_group)
  invisible(ggplotGrob(violin_plot))

  expect_equal(violin_tracedump, global_ggtrace())

  clear_global_ggtrace()

})
