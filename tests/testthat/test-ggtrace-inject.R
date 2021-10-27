library(ggplot2)

global_ggtrace_state(TRUE)

gguntrace(Stat$compute_panel)
boxplot_plot <- ggplot(iris, aes(Species, Sepal.Length)) + geom_boxplot()

test_that("injections modify runtime env (expr in place)", {

  ggtrace(
    Stat$compute_panel,
    trace_steps = 4,
    trace_exprs = quote(groups),
    verbose = FALSE
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
    verbose = FALSE
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
    verbose = FALSE
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
    verbose = FALSE
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
    verbose = FALSE
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
    verbose = FALSE
  )
  invisible(ggplotGrob(boxplot_plot))
  boxplot_tracedump <- last_ggtrace()
  expect_true(isFALSE("injected_var" %in% boxplot_tracedump[[1]]))
  expect_true(isFALSE("injected_var" %in% ls(envir = boxplot_tracedump[[2]])))
  expect_null(boxplot_tracedump[[2]]$injected_var)

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
    verbose = FALSE
  )
  modified_boxplot_data1 <- layer_data(boxplot_plot)
  expect_message(gguntrace(Stat$compute_layer), "not currently being traced")
  boxplot_data2 <- layer_data(ggplot(iris, aes(Species, Sepal.Length * 10)) + geom_boxplot())
  expect_equal(modified_boxplot_data1, boxplot_data2)

  statuses <- last_ggtrace()
  expect_equal(
    vapply(statuses, function(x) "modified" %in% x, logical(1)),
    c(create = TRUE, assign_remove = FALSE, check = FALSE)
  )

  ggtrace(
    Stat$compute_panel,
    trace_steps = c(4, 4),
    trace_exprs = rlang::exprs(
      ls = ls(),
      create_assign = {
        groups <- local({
          modified <- lapply(groups, function(group) {
            group_copy <- group
            group_copy$y <- group_copy$y * 10
            group_copy
          })
          modified
        })
        ls()
      }),
    verbose = FALSE
  )
  modified_boxplot_data2 <- layer_data(boxplot_plot)
  expect_message(gguntrace(Stat$compute_layer), "not currently being traced")
  expect_equal(modified_boxplot_data1, modified_boxplot_data2)

  statuses <- last_ggtrace()
  expect_equal(statuses[[1]], statuses[[2]])

})

test_that("injections can modify conditionally", {

  expect_null(clear_global_ggtrace())

  ggtrace(
    StatBoxplot$compute_group,
    trace_steps = 6,
    trace_exprs = list(result = quote(stats)),
    once = FALSE,
    verbose = FALSE
  )
  print(boxplot_plot)
  gguntrace(StatBoxplot$compute_group)

  boxplot_tracedump_inspect_result <- unname(lapply(global_ggtrace(), `[[`, "result"))
  expect_null(clear_global_ggtrace())

  ggtrace(
    StatBoxplot$compute_group,
    trace_steps = c(4, 4, 6),
    trace_exprs = list(
      cond = quote(data$group[1] == 2),
      inject = quote({
        if (data$group[1] == 2) {
          qs <- c(0, 0.05, 0.5, 0.95, 1)
        } else {
          qs
        }
      }),
      result = quote(stats)
    ),
    once = FALSE,
    verbose = FALSE
  )
  print(boxplot_plot)
  gguntrace(StatBoxplot$compute_group)

  boxplot_tracedump_inject <- global_ggtrace()
  boxplot_tracedump_inject_result <- lapply(boxplot_tracedump_inject, `[[`, "result")
  expect_null(clear_global_ggtrace())

  # condition triggered only for group 2
  expect_equal(
    vapply(boxplot_tracedump_inject, `[[`, logical(1), "cond", USE.NAMES = FALSE),
    c(FALSE, TRUE, FALSE)
  )

  # only different at group 2
  expect_equal(boxplot_tracedump_inspect_result[[1]], boxplot_tracedump_inject_result[[1]])
  expect_equal(boxplot_tracedump_inspect_result[[3]], boxplot_tracedump_inject_result[[3]])
  expect_false(identical(boxplot_tracedump_inspect_result[[2]], boxplot_tracedump_inject_result[[2]]))

})

test_that("injections mutate method of self but safe if copy is replaced for self", {

  expect_equal(StatBoxplot$required_aes, "y|x")
  ggtrace(Stat$compute_panel, 4, quote(self$required_aes <- "y|x|shape"))
  invisible(ggplotGrob(boxplot_plot))
  expect_equal(StatBoxplot$required_aes, "y|x|shape")
  StatBoxplot$required_aes <- "y|x"

  expect_equal(StatBoxplot$required_aes, "y|x")
  StatBoxplot_new <- rlang::env_clone(StatBoxplot)
  class(StatBoxplot_new) <- class(StatBoxplot)
  StatBoxplot_new$required_aes <- "y|x|shape"
  ggtrace(Stat$compute_panel, 4, rlang::expr(self <- !!StatBoxplot_new))
  invisible(ggplotGrob(boxplot_plot))
  expect_equal(StatBoxplot$required_aes, "y|x")

})

test_that("injection modify property of self but safe if copy is replaced for self", {

  # Adopted from {ggplot2} Github issue #4155

  p <- ggplot(data.frame(value = 16)) +
    geom_point(aes(stage(value, after_stat = x), 0), colour = "black", size = 10) +
    geom_point(aes(value, 0), colour = "red", size = 10) +
    scale_x_sqrt(limits = c(0, 16), breaks = c(0, 4, 16))
  p_data <- ggplot_build(p)$data
  expect_equal(p_data[[1]]$x, 2)
  expect_equal(p_data[[2]]$x, 4)
  expect_true(StatIdentity$retransform)

  expect_true(StatIdentity$retransform)
  ggtrace(ggplot2:::Layer$map_statistic, 20, quote(self$stat$retransform <- FALSE), verbose = FALSE)
  p_no_retransform_data <- ggplot_build(p)$data
  expect_equal(p_no_retransform_data[[1]]$x, 4)
  expect_equal(p_no_retransform_data[[1]]$x, p_data[[2]]$x)
  expect_true(isFALSE(StatIdentity$retransform))
  StatIdentity$retransform <- TRUE

  expect_true(StatIdentity$retransform)
  StatIdentity_new <- rlang::env_clone(StatBoxplot)
  class(StatIdentity_new) <- class(StatIdentity)
  StatIdentity_new$retransform <- FALSE
  ggtrace(ggplot2:::Layer$map_statistic, 20, rlang::expr(self$stat <- !!StatIdentity_new), verbose = FALSE)
  p_no_retransform_copy_data <- ggplot_build({
    ggplot(data.frame(value = 16)) +
      geom_point(aes(stage(value, after_stat = x), 0), colour = "black", size = 10) +
      geom_point(aes(value, 0), colour = "red", size = 10) +
      scale_x_sqrt(limits = c(0, 16), breaks = c(0, 4, 16))
  })$data
  expect_equal(p_no_retransform_copy_data[[1]]$x, 4)
  expect_equal(p_no_retransform_copy_data[[1]]$x, p_data[[2]]$x)
  expect_true(StatIdentity$retransform)
  # overriding `self()` is ok ...
  expect_true(geom_point()$stat$retransform)
  expect_true(geom_point(aes(stage(value, after_stat = x), 0), colour = "black", size = 10)$stat$retransform)
  expect_true(p$layers[[1]]$stat$retransform)
  # ... because layer environment is created anew each time it's called
  expect_true(isFALSE(identical(geom_point(), geom_point())))
  expect_true(rlang::env_label(geom_point()) != rlang::env_label(geom_point()))


})

test_that("injections can be conditional", {

  clear_global_ggtrace()
  ggtrace(
    StatBoxplot$compute_group,
    c(16, 16),
    rlang::exprs(
      condition = data$group[1] == 2,
      injection = if (data$group[1] == 2) {
        width <- width/2
      }
    ),
    once = FALSE,
    verbose = FALSE
  )
  boxplot_conditional <- layer_data(boxplot_plot)
  gguntrace(StatBoxplot$compute_group)
  expect_equal(vapply(global_ggtrace(), `[[`, logical(1), "condition", USE.NAMES = FALSE), c(FALSE, TRUE, FALSE))
  expect_equal(boxplot_conditional$new_width[1], boxplot_conditional$new_width[3])
  expect_equal(boxplot_conditional$new_width[1]/2, boxplot_conditional$new_width[2])

  expect_message(gguntrace(StatBoxplot$compute_group), "not currently being traced")
  boxplot_normal <- layer_data(boxplot_plot)
  expect_true(length(unique(boxplot_normal$new_width)) == 1)

})

test_that("printing output does not evaluate exprs twice", {
  aaa <- function() {
    a <- 1
    b <- 1
    c <- 1
    a + b + c
  }
  original <- aaa()

  ggtrace(aaa, -1, quote(a <- a + 10), verbose = FALSE)
  no_print <- aaa()
  expect_equal(no_print, 13)
  expect_equal(no_print, original + 10)

  ggtrace(aaa, -1, quote(a <- a + 10), verbose = FALSE)
  yes_print <- aaa()
  expect_equal(yes_print, no_print)
})

global_ggtrace_state(FALSE)
