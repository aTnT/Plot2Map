library(testthat)
library(Plot2Map)
library(terra)
library(gstat)

# Tests for SpatCor.R

# Unit test with real-world raster
test_that("SpatCor with AGB raster works", {
  # Create a test directory if it doesn't exist
  test_dir <- "tests/test_data"
  if (!dir.exists(test_dir)) {
    dir.create(test_dir, recursive = TRUE)
  }

  # Construct the raster file path
  raster_path <- file.path(test_dir, "N40W010_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2021-fv5.0.tif")

  # Check if the raster file exists; skip the test if not found
  if (!file.exists(raster_path)) {
    skip("Raster file not found, skipping test")
  }

  # Load the raster
  r <- terra::rast(raster_path)

  # Crop the raster to a smaller extent for efficiency
  small_ext <- terra::ext(-9.3, -9, 39, 39.3)
  r_small <- terra::crop(r, small_ext)

  # Define a simple variogram model for testing
  vgm_model <- gstat::vgm(psill = 1, model = "Sph", range = 100, nugget = 0.1)

  result <- SpatCor(vgm_model, r_small)

  expect_equal(mean(terra::values(result)), 21.72897, tolerance = 1e-6)
  expect_equal(length(terra::values(result)), 9)
})