context("testing feature extraction functions")

poly <- sf::st_polygon(list(matrix(c(-89,-89.5,-89,-88.5,-89, 42,42,44,44,42), ncol = 2)))
poly_sfc <- sf::st_sfc(poly, crs = 4326)
poly_df <- sf::st_sf(poly_sfc, name = 'poly_1')

point_sfc <- sf::st_sfc(sf::st_point(c(-89,42)), crs = 4326)
point_df <-  sf::st_sf(point_sfc, name = 'point_1')

points_sfc <- sf::st_sfc(sf::st_multipoint(matrix(c(-89, -108, -154, 42, 33, 65.58), ncol = 2)), crs = 4326)
PR_points_sfc <- sf::st_sfc(sf::st_multipoint(matrix(c(-66.562926, -65.483519, -67.897202, 18.3, 18.12, 18.098), ncol = 2)), crs = 4326)

ak_poly <- sf::st_polygon(list(matrix(c(-148.90062769206,-148.920356797163,-148.945765922973,-148.944816863298,-148.94381443877,-148.90062769206,
                                          60.3686995983039,60.3673520719981,60.3659310258993,60.3827950517552,60.4178583481427,60.3686995983039), ncol = 2)))
ilak_catchment <- sf::st_sf(sf::st_sfc(ak_poly,poly, crs = 4326))



test_that("feature types are correct", {
  expect_equal(meddle:::feature_type(poly_sfc)[['feature-type']], "G-polygon")
  expect_equal(meddle:::feature_type(poly_df)[['feature-type']], "G-polygon")
  expect_equal(meddle:::feature_type(point_sfc)[['feature-type']], "Point")
  expect_equal(meddle:::feature_type(points_sfc)[['feature-type']], "Point")
  expect_equal(meddle:::feature_type(point_df)[['feature-type']], "Point")
})


test_that("feature bounding boxes are correct", {
  bb.point <- meddle:::feature_bbox(point_sfc)
  bb.poly <- meddle:::feature_bbox(poly_sfc)
  expect_equal(bb.poly, meddle:::feature_bbox(poly_df))
  expect_equal(bb.point, meddle:::feature_bbox(point_df))
  expect_true(all(names(bb.point) %in% c("wbbox", "ebbox", "nbbox", "sbbox")))
  expect_true(bb.poly$nbbox > bb.poly$sbbox)
  expect_equal(bb.point$wbbox, bb.point$ebbox)
  expect_false(bb.poly$wbbox == bb.poly$ebbox)
  expect_equal(meddle:::feature_bbox(points_sfc)$wbbox, -154)
})

test_that("feature counts are correct", {
  expect_equal(meddle:::feature_count(poly_sfc)[['feature-count']], 1)
  expect_equal(meddle:::feature_count(poly_df)[['feature-count']], 1)
  expect_equal(meddle:::feature_count(point_df)[['feature-count']], 1)
  expect_equal(meddle:::feature_count(points_sfc)[['feature-count']], 3)
})

test_that("feature overlap with states are correct", {
  state.poly <- sapply(meddle:::feature_states(poly_df)[['states']],function(x) x[['state-name']])
  state.point <- sapply(meddle:::feature_states(point_sfc)[['states']],function(x) x[['state-name']])
  state.points <- sapply(meddle:::feature_states(points_sfc)[['states']],function(x) x[['state-name']])
  state.polys <- sapply(meddle:::feature_states(ilak_catchment)[['states']],function(x) x[['state-name']])
  expect_equal(state.poly, c("Illinois", "Wisconsin"))
  expect_equal(state.point, c("Illinois"))
  expect_equal(state.points, c("Alaska", "Illinois", "New Mexico"))
  expect_equal(state.polys, c("Alaska", "Illinois", "Wisconsin"))
  expect_equal(meddle:::feature_states(PR_points_sfc)$states[[1]][['state-name']], "Puerto Rico")
  
})
