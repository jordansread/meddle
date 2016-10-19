context("testing feature extraction functions")

Sr1 = sp::Polygon(cbind(c(-89,-89.5,-89,-88.5,-89),c(42,42,44,44,42)))
Srs1 = sp::Polygons(list(Sr1), "s1")
s.poly = sp::SpatialPolygons(list(Srs1), proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
df <- data.frame('name','poly1')
row.names(df) <- row.names(s.poly)
s.poly.df <- sp::SpatialPolygonsDataFrame(s.poly, data = df)

s.point <- sp::SpatialPoints(cbind(-89,42), proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
s.point.df <- sp::SpatialPointsDataFrame(cbind(-89,42), proj4string=sp::CRS("+proj=longlat +datum=WGS84"), data = data.frame('name','point1'))

s.points <- sp::SpatialPoints(cbind(c(-89, -108, -154),c(42, 33, 65.58)), proj4string=sp::CRS("+proj=longlat +datum=WGS84"))


test_that("feature types are correct", {
  expect_equal(meddle:::feature_type(s.poly)[['feature-type']], "G-polygon")
  expect_equal(meddle:::feature_type(s.poly.df)[['feature-type']], "G-polygon")
  expect_equal(meddle:::feature_type(s.point)[['feature-type']], "Point")
  expect_equal(meddle:::feature_type(s.point.df)[['feature-type']], "Point")
})


test_that("feature bounding boxes are correct", {
  bb.point <- meddle:::feature_bbox(s.point)
  bb.poly <- meddle:::feature_bbox(s.poly)
  expect_equal(bb.poly, meddle:::feature_bbox(s.poly.df))
  expect_equal(bb.point, meddle:::feature_bbox(s.point))
  expect_true(all(names(bb.point) %in% c("wbbox", "ebbox", "nbbox", "sbbox")))
  expect_equal(bb.point$wbbox, bb.point$ebbox)
  expect_false(bb.poly$wbbox == bb.poly$ebbox)
  expect_equal(meddle:::feature_bbox(s.points)$wbbox, -154)
})

test_that("feature counts are correct", {
  expect_equal(meddle:::feature_count(s.poly)[['feature-count']], 1)
  expect_equal(meddle:::feature_count(s.point)[['feature-count']], 1)
  expect_equal(meddle:::feature_count(s.points)[['feature-count']], 3)
})

test_that("feature overlap with states are correct", {
  state.poly <- sapply(meddle:::feature_states(s.poly)[['states']],function(x) x[['state-name']])
  state.point <- sapply(meddle:::feature_states(s.point)[['states']],function(x) x[['state-name']])
  state.points <- sapply(meddle:::feature_states(s.points)[['states']],function(x) x[['state-name']])
  expect_equal(state.poly, c("Illinois", "Wisconsin"))
  expect_equal(state.point, c("Illinois"))
  expect_equal(state.points, c("Illinois", "New Mexico"))
})
