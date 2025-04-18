library(ggplot2)

global_ggtrace_state(TRUE)

test_that("inspection workflow works (Geom)", {

  clear_last_ggtrace()
  expect_null(last_ggtrace())

  smooth_plot <- ggplot(mtcars, aes(mpg, hp)) +
    geom_point() +
    stat_smooth(method = "loess", formula = y ~ x)

  ggtrace(
    method = GeomSmooth$draw_group,
    trace_steps = -1,           # Trace the last line
    trace_exprs = quote(~step), # Grab the gList() object it returns
    print_output = FALSE,
    verbose = FALSE
  )

  invisible(ggplotGrob(smooth_plot))

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

test_that("aes_eval vignette", {

  global_ggtrace_state(TRUE)

  # Bar plot using computed/"mapped" aesthetics with `after_stat()` and `after_scale()`
  barplot_plot <- ggplot(data = iris) +
    geom_bar(
      mapping = aes(
        x = Species,                           # Discrete x-axis representing species
        y = after_stat(count / sum(count)),    # Bars represent count of species as proportions
        color = Species,                       # The outline of the bars are colored by species
        fill = after_scale(alpha(color, 0.5))  # The fill of the bars are lighter than the outline color
      ),
      linewidth = 3
    )

  ggtrace(
    method = ggplot2:::Layer$map_statistic, # Layer is not exported so need the `:::`
    trace_steps = c(1, -1),
    trace_exprs = rlang::exprs(
      before = data,            # What does the data look like BEFORE resolving `after_stat()`?
      after = ~step             # What does the data look like AFTER resolving `after_stat()`?
      # - The `~step` keyword runs the step and returns its output
    ),
    verbose = FALSE
  )

  ggtrace(
    method = Geom$use_defaults,
    trace_steps = c(6, 7),
    trace_exprs = rlang::exprs(
      before = data,            # What does the data look like BEFORE resolving `after_scale()`
      after = data              # What does the data look like AFTER resolving `after_scale()`
    ),
    verbose = FALSE
  )

  clear_global_ggtrace()
  invisible(ggplot_build(barplot_plot))

  tracedump <- global_ggtrace()
  expect_true(grepl("ggplot2:::Layer\\$map_statistic", names(tracedump)[1]))
  expect_true(grepl("Geom\\$use_defaults", names(tracedump)[2]))

  names(tracedump) <- c("after_stat", "after_scale")
  expect_equal(names(tracedump[[1]]), c("before", "after"))
  expect_equal(names(tracedump[[1]]), names(tracedump[[2]]))

  expect_equal(
    tracedump$after_stat$before$count / sum(tracedump$after_stat$before$count),
    tracedump$after_stat$after$y
  )
#
#   # Return `self` inside the method
#   # `self` should be contextualized to the Layer and the Geom
#   clear_global_ggtrace()
#   ggtrace(ggplot2:::Layer$compute_geom_2, 1, quote(self), verbose = FALSE)
#   ggtrace(Geom$use_defaults, 1, quote(self), verbose = FALSE)
#
#   # Force evaluation of plot code without printing it
#   invisible(ggplot_build(barplot_plot))
#
#   traced_self <- unlist(global_ggtrace(), recursive = FALSE)
#   names(traced_self) <- c("layer", "geom")
#
#   expect_equal(traced_self$geom, GeomBar)
#   expect_equal(traced_self$geom, geom_bar()$geom)
#   expect_equal(traced_self$geom, traced_self$layer$geom)
#   expect_equal(traced_self$layer$stat, StatCount)
#   expect_equal(traced_self$layer$stat, geom_bar()$stat)

})

global_ggtrace_state(FALSE)

# test_that("is.ggtrace_placeholder class checking", {
#   expect_no_warning(
#     l <- ggtrace_inspect_return(
#       x = ggplot(mtcars, aes(mpg, hp)) +
#         geom_point(aes(color = as.factor(cyl))),
#       method = ggplot2:::guide_gengrob.legend, cond = 1
#     )
#   )
#   expect_equal(class(l), c("gtable", "gTree", "grob", "gDesc"))
# })
