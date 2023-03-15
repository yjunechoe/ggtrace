test_that("get_method() edge case for top-level ggproto", {
  expect_equal(
    object = list(Geom = sort(names(ggplot2::Geom))),
    expected = get_method_inheritance(ggplot2::Geom)
  )
})
