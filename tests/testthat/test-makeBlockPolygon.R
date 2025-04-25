library(testthat)
library(Plot2Map)
library(sf)
library(terra)

# Tests for MakeBlockPolygon.R

# Test internal consistency
test_that("MakeBlockPolygon function behaves consistently", {
  x <- 5
  y <- 10
  size <- 1

  result <- MakeBlockPolygon(x, y, size)

  # Check output structure
  expect_s3_class(result, "sf")
  expect_true("geometry" %in% names(result))

  # Check if the polygon is square
  bbox <- st_bbox(result)
  expect_equal(as.numeric(- bbox["xmin"] + bbox["xmax"]), as.numeric(- bbox["ymin"] + bbox["ymax"]))

  # Check if the polygon size is correct
  expect_equal(as.numeric(bbox["xmax"] - bbox["xmin"]), size)

  # Check CRS
  expect_equal(st_crs(result)$epsg, 4326)
})