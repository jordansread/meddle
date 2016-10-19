context("testing attribute methods")

test_that("attribute table creation", {
  df <- meddle:::create_attr_table(c('x','y','x'))
  expect_is(df, 'data.frame')
})

test_that("attr_names for different classes", {
  df <- data.frame('dog'='harry','cat'='betty', stringsAsFactors=FALSE)
  expect_equal(attr_names(df), c('dog','cat'))

  expect_is(attr_names(system.file(package='meddle','extdata','example_shapefile')), 'character')
  sp <- read_data(system.file(package='meddle','extdata','example_shapefile'))
  expect_equal(attr_names(system.file(package='meddle','extdata','example_shapefile')), attr_names(sp))
})
