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

test_that("environment properties are as expected", {

  ggtrace:::set_last_ggtrace(NULL)
  expect_null(last_ggtrace())

  boxplot_plot <- ggplot(diamonds[1:500,], aes(cut, depth)) +
    geom_boxplot()

  ggtrace(
    Stat$compute_panel,
    trace_steps = c(1, 1, 1, -1, -1),
    trace_exprs = rlang::exprs(
      beginning = mget(ls()),
      env_start = environment(),
      env_deep_start = rlang::env_clone(environment()),
      env_end = environment(),
      env_deep_end = rlang::env_clone(environment())
    ),
    .print = FALSE
  )

  print(boxplot_plot)
  boxplot_tracedump <- last_ggtrace()

  # named tracedump
  step_names <- c("beginning", "env_start", "env_deep_start", "env_end", "env_deep_end")
  expect_true(
    all(mapply(function(el, nm) {grepl(nm, el)}, names(boxplot_tracedump), step_names, USE.NAMES = FALSE))
  )
  names(boxplot_tracedump) <- step_names
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
    .print = FALSE
  )

  print(boxplot_plot)
  boxplot_tracedump2 <- last_ggtrace()

  # new environment created at runtime; not affected by modification to previously retrieved runtime env
  expect_true(
    rlang::env_label(boxplot_tracedump2[[1]]) != rlang::env_label(boxplot_tracedump[["env_start"]])
  )
  expect_null(
    boxplot_tracedump2[[1]]$a
  )
  expect_true(
    all(boxplot_tracedump[["env_start"]]$a == 1, boxplot_tracedump[["env_end"]]$a == 1)
  )

  # possible to manually execute steps of the body with the environment
  expect_true(
    is.list(boxplot_tracedump2[[2]])
  )
  expect_equal(
    eval(
      as.list(body(get("compute_panel", Stat)))[[6]],
      boxplot_tracedump2[[1]]
    )[,1:12],
    ggplot2:::rbind_dfs(boxplot_tracedump2[[2]])
  )

  gguntrace(Stat$compute_panel)
})

# TODO test injections
# TODO test injections with !!
# TODO behavior of persistence trace
