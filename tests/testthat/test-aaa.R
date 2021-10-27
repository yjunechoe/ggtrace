test_that("global tracedump turned off at start", {
  expect_true(isFALSE(global_ggtrace_state()))
})
