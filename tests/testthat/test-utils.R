library(ggplot2)

test_that("parts of ggproto method correctly extracted from expression", {
  expect_equal(
    split_ggproto_method(StatCount$compute_group),
    list(
      method_name = "compute_group",
      obj = StatCount,
      obj_name = "StatCount",
      ns = "",
      formatted_call = "StatCount$compute_group"
    )
  )
})

test_that("namespace is captured correctly", {
  expect_equal(split_ggproto_method(ggplot2::StatCount$compute_group)$ns, "ggplot2")
  expect_equal(split_ggproto_method(ggplot2:::Layer$compute_statistic)$ns, "ggplot2")
})

test_that("handle private variables differently from methods", {

  method_fn <- get("compute_group", StatCount)
  expect_equal(resolve_method(method_fn), as.list(body(method_fn)))

  private_var <- get("default_aes", StatCount)
  expect_equal(resolve_method(private_var), private_var)

})

test_that("is_traced works correctly", {

  ggtrace(Stat$compute_layer, 1)
  expect_true(is_traced(Stat$compute_layer))
  gguntrace(Stat$compute_layer)
  expect_false(is_traced(Stat$compute_layer))

})
