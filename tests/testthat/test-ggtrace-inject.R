library(ggplot2)

gguntrace(Stat$compute_panel)
boxplot_plot <- ggplot(iris, aes(Species, Sepal.Length)) + geom_boxplot()

test_that("injections modify runtime env (expr in place)", {

  ggtrace(
    Stat$compute_panel,
    trace_steps = 4,
    trace_exprs = quote(groups),
    print_output = FALSE
  )
  invisible(ggplotGrob(boxplot_plot))
  as_is <- last_ggtrace()[[1]]


  ggtrace(
    Stat$compute_panel,
    trace_steps = 4,
    trace_exprs = rlang::expr({
      groups <- lapply(groups, function(group) {
        group_copy <- group
        group_copy$y <- group_copy$y * 10
        group_copy
      })
    }),
    print_output = FALSE
  )
  modified_boxplot_data <- layer_data(boxplot_plot)
  expect_message(gguntrace(Stat$compute_layer), "not currently being traced")
  boxplot_data <- layer_data(boxplot_plot)
  boxplot_data2 <- layer_data(ggplot(iris, aes(Species, Sepal.Length * 10)) + geom_boxplot())

  expect_equal(modified_boxplot_data, boxplot_data2)

})

test_that("injections modify runtime env (bang-bang substitution)", {

  ggtrace(
    Stat$compute_panel,
    trace_steps = 4,
    trace_exprs = quote(groups),
    print_output = FALSE
  )
  invisible(ggplotGrob(boxplot_plot))
  as_is <- last_ggtrace()[[1]]

  modify <- lapply(as_is, function(group) {
    group_copy <- group
    group_copy$y <- group_copy$y * 10
    group_copy
  })

  ggtrace(
    Stat$compute_panel,
    trace_steps = 4,
    trace_exprs = rlang::expr(groups <- !!modify),
    print_output = FALSE
  )
  modified_boxplot_data <- layer_data(boxplot_plot)
  expect_message(gguntrace(Stat$compute_layer), "not currently being traced")
  boxplot_data2 <- layer_data(ggplot(iris, aes(Species, Sepal.Length * 10)) + geom_boxplot())

  expect_equal(modified_boxplot_data, boxplot_data2)

})

test_that("injected var available in later step of same runtime env", {

  ggtrace(
    Stat$compute_panel,
    trace_steps = 3:5,
    trace_exprs = rlang::exprs(
      injected_var <- "hello",
      injected_var,
      environment()
    ),
    print_output = FALSE
  )
  invisible(ggplotGrob(boxplot_plot))
  boxplot_tracedump <- last_ggtrace()

  expect_equal(boxplot_tracedump[1], boxplot_tracedump[2])
  expect_true("injected_var" %in% ls(envir = boxplot_tracedump[[3]]))
  expect_equal(boxplot_tracedump[[1]], boxplot_tracedump[[3]]$injected_var)

})

test_that("injection doesn't persist across diff calls to the same method", {

  ggtrace(
    Stat$compute_panel,
    trace_steps = 3:5,
    trace_exprs = rlang::exprs(
      injected_var <- "hello",
      injected_var,
      environment()
    ),
    print_output = FALSE
  )
  invisible(ggplotGrob(boxplot_plot))
  boxplot_tracedump <- last_ggtrace()

  expect_equal(boxplot_tracedump[1], boxplot_tracedump[2])
  expect_true("injected_var" %in% ls(envir = boxplot_tracedump[[3]]))
  expect_equal(boxplot_tracedump[[1]], boxplot_tracedump[[3]]$injected_var)

  ggtrace(
    Stat$compute_panel,
    trace_steps = c(1, 1),
    trace_exprs = rlang::exprs(
      ls(),
      environment()
    ),
    print_output = FALSE
  )
  invisible(ggplotGrob(boxplot_plot))
  boxplot_tracedump <- last_ggtrace()
  expect_true(!"injected_var" %in% boxplot_tracedump[[1]])
  expect_true(!"injected_var" %in% ls(envir = boxplot_tracedump[[2]]))
  expect_null(boxplot_tracedump[[2]]$injected_var)

})

test_that("injection doesn't persist with once = FALSE", {

})

test_that("injections can clean up locally defined variables", {

  ggtrace(
    Stat$compute_panel,
    trace_steps = c(4, 4, -1),
    trace_exprs = rlang::exprs(
      create = {
        modified <- lapply(groups, function(group) {
          group_copy <- group
          group_copy$y <- group_copy$y * 10
          group_copy
        })
        ls()
      },
      assign_remove = {
        groups <- modified
        rm(modified)
        ls()
      },
      check = ls()
    ),
    print_output = FALSE
  )
  modified_boxplot_data <- layer_data(boxplot_plot)
  expect_message(gguntrace(Stat$compute_layer), "not currently being traced")
  boxplot_data2 <- layer_data(ggplot(iris, aes(Species, Sepal.Length * 10)) + geom_boxplot())

  expect_equal(modified_boxplot_data, boxplot_data2)

  statuses <- last_ggtrace()
  expect_equal(
    vapply(statuses, function(x) "modified" %in% x, logical(1)),
    c(create = TRUE, assign_remove = FALSE, check = FALSE)
  )

})
