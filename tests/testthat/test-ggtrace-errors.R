library(ggplot2)

test_that("step and exprs mismatch", {

  expect_message(
    gguntrace(Stat$compute_panel),
    "not currently being traced"
  )

  expect_error(
    ggtrace(
      Stat$compute_panel,
      trace_steps = c(1, 1, -1, -1), # four steps but 5 exprs
      trace_exprs = rlang::exprs(
        beginning = mget(ls()),
        env_start = environment(),
        env_deep_start = rlang::env_clone(environment()),
        env_end = environment(),
        env_deep_end = rlang::env_clone(environment())
      ),
      print_output = FALSE
    ),
    "Length mismatch"
  )

  expect_message(
    gguntrace(Stat$compute_panel),
    "not currently being traced"
  )

})
