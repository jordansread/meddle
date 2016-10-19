context("testing feature extraction functions")

Sr1 = sp::Polygon(cbind(c(-89,-89.5,-89,-88.5,-89),c(42,42,44,44,42)))
Srs1 = sp::Polygons(list(Sr1), "s1")
s.poly = sp::SpatialPolygons(list(Srs1), proj4string=CRS("+proj=longlat +datum=WGS84"))
s.poly.df <- sp::SpatialPolygonsDataFrame(list(Srs1), data = data.frame('name','poly1'))

s.points <- sp::SpatialPoints(cbind(-89,42), proj4string=CRS("+proj=longlat +datum=WGS84"))
s.points.df <- sp::SpatialPointsDataFrame(cbind(-89,42), proj4string=CRS("+proj=longlat +datum=WGS84"), data = data.frame('name','point1'))

test_that("feature types are correct", {
  expect_equal(meddle:::feature_type(s.poly), "G-polygon")
  expect_equal(meddle:::feature_type(s.poly.df), "G-polygon")
  expect_equal(meddle:::feature_type(s.point), "Point")
  expect_equal(meddle:::feature_type(s.point.df), "Point")
})
