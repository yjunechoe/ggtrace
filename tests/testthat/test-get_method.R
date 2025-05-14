test_that("get_method() edge case for top-level ggproto", {
  expect_identical(
    object = list(Geom = ls(envir = ggplot2::Geom)),
    expected = get_method_inheritance(ggplot2::Geom)
  )
})

test_that("get_method() edge case for instances (vs. subclass) of class (#107)", {
  expect_identical(
    names(get_method_inheritance(ggplot2::position_jitter())),
    names(get_method_inheritance(ggplot2::PositionJitter))
  )
})
