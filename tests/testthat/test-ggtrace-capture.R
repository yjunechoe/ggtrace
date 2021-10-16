library(ggplot2)

test_that("environment properties are as expected", {

  ggtrace:::set_last_ggtrace(NULL)
  expect_null(last_ggtrace())
  expect_null(clear_global_ggtrace())

  boxplot_plot <- ggplot(diamonds[1:500,], aes(cut, depth)) +
    geom_boxplot()

  boxplot_trace_exprs <- rlang::exprs(
    beginning = mget(ls()),
    env_start = environment(),
    env_deep_start = rlang::env_clone(environment()),
    env_end = environment(),
    env_deep_end = rlang::env_clone(environment())
  )

  ggtrace(
    Stat$compute_panel,
    trace_steps = c(1, 1, 1, -1, -1),
    trace_exprs = boxplot_trace_exprs,
    print_output = FALSE
  )
  invisible(ggplotGrob(boxplot_plot))
  boxplot_tracedump <- last_ggtrace()

  # named tracedump
  expect_equal(names(boxplot_tracedump), names(boxplot_trace_exprs))

  # last 4 are environments
  expect_true(
    all(vapply(boxplot_tracedump[-1], rlang::is_environment, logical(1)))
  )

  # environment() references same environment
  expect_equal(
    boxplot_tracedump[["env_start"]],
    boxplot_tracedump[["env_end"]]
  )
  expect_equal(
    rlang::env_label(boxplot_tracedump[["env_start"]]),
    rlang::env_label(boxplot_tracedump[["env_end"]])
  )

  # deep copy at end shares same state as the runtime environment at the end
  expect_equal(
    mget(ls(boxplot_tracedump[["env_end"]]), envir = boxplot_tracedump[["env_end"]]),
    mget(ls(boxplot_tracedump[["env_deep_end"]]), envir = boxplot_tracedump[["env_deep_end"]])
  )
  # deep copy at the beginning shares same state as snapshot of beginning
  expect_equal(
    boxplot_tracedump[["beginning"]],
    mget(ls(boxplot_tracedump[["env_deep_start"]]), envir = boxplot_tracedump[["env_deep_start"]])
  )

  # modifying the runtime environment after taking it out affects both references
  boxplot_tracedump[["env_start"]]$a <- 1
  expect_equal(
    boxplot_tracedump[["env_start"]]$a,
    boxplot_tracedump[["env_end"]]$a
  )

  # but this doesn't affect any future runs
  gguntrace(Stat$compute_panel)
  ggtrace(
    Stat$compute_panel,
    trace_steps = c(1, 5),
    trace_exprs = list(
      quote(environment()),
      quote(stats)
    ),
    print_output = FALSE
  )

  invisible(ggplotGrob(boxplot_plot))
  boxplot_tracedump2 <- last_ggtrace()

  # new environment created at runtime; not affected by modification to previously retrieved runtime env
  expect_false(rlang::env_label(boxplot_tracedump2[[1]]) == rlang::env_label(boxplot_tracedump[["env_start"]]))
  expect_null(boxplot_tracedump2[[1]]$a)
  expect_true(all(boxplot_tracedump[["env_start"]]$a == 1, boxplot_tracedump[["env_end"]]$a == 1))

  # possible to manually execute steps of the body with the environment
  expect_true(is.list(boxplot_tracedump2[[2]]))
  expect_equal(
    eval(as.list(body(get("compute_panel", Stat)))[[6]], boxplot_tracedump2[[1]])[,1:12],
    ggplot2:::rbind_dfs(boxplot_tracedump2[[2]])
  )

  gguntrace(Stat$compute_panel)
  expect_null(clear_global_ggtrace())

})
