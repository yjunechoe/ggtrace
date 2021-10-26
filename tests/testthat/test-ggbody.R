library(ggplot2)

StatDensityCommon <- ggproto("StatDensityCommon", Stat,
                             required_aes = "x",

                             setup_params = function(data, params) {
                               if (!is.null(params$bandwidth))
                                 return(params)

                               xs <- split(data$x, data$group)
                               bws <- vapply(xs, bw.nrd0, numeric(1))
                               bw <- mean(bws)
                               message("Picking bandwidth of ", signif(bw, 3))

                               params$bandwidth <- bw
                               params
                             },

                             compute_group = function(data, scales, bandwidth = 1) {
                               d <- density(data$x, bw = bandwidth)
                               data.frame(x = d$x, y = d$y)
                             }
)

test_that("ggbody gets body as list", {
  expect_equal(
    ggbody(GeomRect$draw_panel),
    as.list(body(get("draw_panel", GeomRect)))
  )
  expect_equal(
    ggbody(StatDensityCommon$compute_group),
    as.list(body(get("compute_group", StatDensityCommon))),
    ignore_attr = TRUE
  )
  expect_equal(
    ggbody(StatDensityCommon$setup_params),
    as.list(body(get("setup_params", StatDensityCommon))),
    ignore_attr = TRUE
  )
})

test_that("warns if already being traced", {
  ggtrace(StatBoxplot$compute_group, 1)
  expect_true(is_traced(StatBoxplot$compute_group))
  expect_warning(ggbody(StatBoxplot$compute_group), "Method is currently being traced")
  expect_message(gguntrace(StatBoxplot$compute_group), "no longer being traced")
  expect_true(!is_traced(StatBoxplot$compute_group))
})

test_that("works with unimported :::", {
  expect_equal(
    ggbody(ggplot2:::Layer$compute_position),
    as.list(body(get("compute_position", ggplot2:::Layer)))
  )
})

test_that("works with unloaded ::",{
  expect_equal(
    ggbody(ggforce::StatBezier$compute_panel),
    as.list(body(get("compute_panel", ggforce::StatBezier)))
  )
})

test_that("errors if method missing or not defined for ggproto object", {
  expect_error(
    ggbody(StatBoxplot$compute_panel),
    "Method .* not defined for .*"
  )
  expect_error(
    ggbody(StatBoxplot$not_a_method),
    "Method .* not defined for .*"
  )
  expect_error(
    ggbody(StatDensityCommon$compute_panel),
    "Method .* not defined for .*"
  )
})

test_that("errors if method not found in parent(s)", {
  expect_error(
    ggbody(StatBoxplot$not_a_method, inherit = TRUE),
    "Method .* not inherited for .*"
  )
  expect_error(
    ggbody(StatDensityCommon$draw_layer, inherit = TRUE),
    "Method .* not inherited for .*"
  )
})

test_that("errors if object not defined", {
  expect_error(
    ggbody(NotAnObj$method),
    "not found"
  )
})

test_that("returns method from closest parent in a message", {
  expect_equal(
    class(GeomAnnotationMap),
    c("GeomAnnotationMap", "GeomMap", "GeomPolygon", "Geom", "ggproto", "gg")
  )
  expect_message(
    ggbody(GeomAnnotationMap$draw_panel, inherit = TRUE),
    "not inherited"
  )
  expect_message(
    ggbody(GeomAnnotationMap$required_aes, inherit = TRUE),
    "GeomMap\\$required_aes"
  )
  expect_message(
    ggbody(GeomAnnotationMap$draw_key, inherit = TRUE),
    "GeomPolygon\\$draw_key"
  )
  expect_message(
    ggbody(GeomAnnotationMap$draw_layer, inherit = TRUE),
    "Geom\\$draw_layer"
  )
  expect_message(
    ggbody(StatDensityCommon$compute_panel, inherit = TRUE),
    "Stat\\$compute_panel"
  )
})

test_that("returns method from closest parent in a message 2", {
  library(ggforce)
  expect_message(
    ggbody(GeomArcBar$default_aes, inherit = TRUE),
    "not inherited"
  )
  expect_message(
    ggbody(GeomArcBar$draw_panel, inherit = TRUE),
    "GeomShape\\$draw_panel"
  )
  expect_message(
    ggbody(GeomArcBar$draw_key, inherit = TRUE),
    "GeomPolygon\\$draw_key"
  )
  expect_message(
    ggbody(GeomArcBar$draw_group, inherit = TRUE),
    "Geom\\$draw_group"
  )
})

test_that("returns same with or without :: and :::", {
  expect_equal(
    ggbody(StatBin$compute_group),
    ggbody(ggplot2::StatBin$compute_group)
  )
  expect_equal(
    ggbody(StatBin$compute_group),
    ggbody(ggplot2:::StatBin$compute_group)
  )
  library(ggforce)
  expect_equal(
    ggbody(FacetCol$draw_panels),
    ggbody(ggforce::FacetCol$draw_panels)
  )
})

test_that("handles non-functions", {
  expect_equal(
    ggbody(PositionJitter$required_aes),
    c("x", "y")
  )
  expect_equal(
    ggbody(GeomBar$extra_params),
    c("na.rm", "orientation")
  )
  expect_equal(
    ggbody(StatDensityCommon$required_aes),
    "x"
  )
})

test_that("method expr must be a call", {
  expect_error(ggbody(Stat), "must be a call")
})

test_that("method expression validation", {
  expect_error(ggbody(ggplot2:::Layer), "Invalid method expression")
  expect_error(ggbody(get("compute_statistic", ggplot2:::Layer)), "must be a variable not a call")
})
