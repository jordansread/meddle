context("testing attribute methods")

test_that("attribute table creation", {
  df <- meddle:::create_attr_table(c('x','y','x'))
  expect_is(df, 'data.frame')
})

test_that("attr_names for different classes", {
  df <- data.frame('dog'='harry','cat'='betty', stringsAsFactors=FALSE)
  expect_equal(attr_names(df), c('dog','cat'))

  expect_is(attr_names(system.file(package='meddle','extdata','example_shapefile')), 'character')
  sf <- read_data(system.file(package='meddle','extdata','example_shapefile'))
  expect_equal(attr_names(system.file(package='meddle','extdata','example_shapefile')), attr_names(sf))
})

context("creating attribute skeleton doesn't override")

test_that("can write a manual attribute table", {
  df <- meddle:::create_attr_table(c('x','y','x'))
  df[1,]$`attr-def` <- 'crazy uncle jim'
  df[1,]$`attr-defs` <- '5824 37th Street Michigan City MI'
  df <- df[1:2, ] # drop z
  attr.file <- tempfile(fileext = '.csv')
  meddle:::write_attr_file(df, attr.file)
  df.read <- meddle:::read_attr_file(attr.file)
  expect_equal(df[1,]$`attr-def`, df.read[1,]$`attr-def`)
})

test_that("can write a manual attribute table and not have changes overwritten", {
  df <- meddle:::create_attr_table(c('x','y','x'))
  df[1,]$`attr-def` <- 'crazy uncle jim'
  df[1,]$`attr-defs` <- '5824 37th Street Michigan City MI'
  df <- df[1:2, ] # drop z
  attr.file <- tempfile(fileext = '.csv')
  meddle:::write_attr_file(df, attr.file)
  attribute_skeleton(system.file(package='meddle','extdata','example_shapefile'), attr.file)
  df.read <- meddle:::read_attr_file(attr.file)
  expect_equal(df[1,]$`attr-def`, df.read[1,]$`attr-def`)
  expect_equal(df.read$`attr-label`, c('x','y','z'))
})

