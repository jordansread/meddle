context("testing feature extraction functions")

Sr1 = sp::Polygon(cbind(c(-89,-89.5,-89,-88.5,-89),c(42,42,44,44,42)))
Srs1 = sp::Polygons(list(Sr1), "s1")
s.poly = sp::SpatialPolygons(list(Srs1), proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
df <- data.frame('name','poly1')
row.names(df) <- row.names(s.poly)
s.poly.df <- sp::SpatialPolygonsDataFrame(s.poly, data = df)

s.point <- sp::SpatialPoints(cbind(-89,42), proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
s.point.df <- sp::SpatialPointsDataFrame(cbind(-89,42), proj4string=sp::CRS("+proj=longlat +datum=WGS84"), data = data.frame('name','point1'))

test_that("feature types are correct", {
  expect_equal(meddle:::feature_type(s.poly)[['feature-type']], "G-polygon")
  expect_equal(meddle:::feature_type(s.poly.df)[['feature-type']], "G-polygon")
  expect_equal(meddle:::feature_type(s.point)[['feature-type']], "Point")
  expect_equal(meddle:::feature_type(s.point.df)[['feature-type']], "Point")
})
