library(ggplot2)

global_ggtrace_state(TRUE)

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
    print_output = FALSE,
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

test_that("global tracedump state works", {

  global_ggtrace_state(TRUE)
  clear_global_ggtrace()
  expect_true(global_ggtrace_state())
  expect_true(isFALSE(global_ggtrace_state(FALSE)))
  expect_true(isFALSE(global_ggtrace_state()))
  expect_message(global_ggtrace_state(FALSE), "deactivated")
  expect_message(global_ggtrace_state(TRUE), "activated")
  expect_true(global_ggtrace_state())

  global_ggtrace_state(FALSE)
  expect_message(global_ggtrace(), "currently turned off")
  ggtrace(GeomViolin$draw_group, -1, verbose = FALSE)
  invisible(ggplotGrob(violin_plot))
  expect_message(global_ggtrace(), "currently turned off")
  expect_null(global_ggtrace())

  global_ggtrace_state(TRUE)
  global_ggtrace_state()
  ggtrace(GeomViolin$draw_group, -1, verbose = FALSE)
  invisible(ggplotGrob(violin_plot))
  expect_message(global_ggtrace(), NA)
  expect_true(!is.null(global_ggtrace()))
  expect_equal(global_ggtrace()[[1]], last_ggtrace())

  clear_global_ggtrace()
  expect_null(global_ggtrace())
  clear_last_ggtrace()
  expect_null(last_ggtrace())
  global_ggtrace_state(FALSE)

})

global_ggtrace_state(FALSE)
