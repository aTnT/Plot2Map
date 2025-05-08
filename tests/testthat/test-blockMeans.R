library(testthat)
library(Plot2Map)
library(sf)
library(terra)

# Tests for BlockMeans.R

test_that("sampleTreeCover correctly handles extract column indexing", {
  # Create a test raster with constant value of 80
  test_raster <- terra::rast(nrows=20, ncols=20, xmin=0, xmax=1, ymin=0, ymax=1, crs="EPSG:4326")
  terra::values(test_raster) <- rep(80, terra::ncell(test_raster))
  
  # Define a test region of interest
  test_roi <- sf::st_polygon(list(rbind(c(0.2, 0.2), c(0.25, 0.2), 
                                      c(0.25, 0.25), c(0.2, 0.25),
                                      c(0.2, 0.2))))
  test_roi_sf <- sf::st_sfc(test_roi, crs = 4326)
  
  # Run with different thresholds
  result <- sampleTreeCover(test_roi_sf, thresholds = c(0, 10, 70, 90), forest_mask = test_raster)
  
  # The expected forest cover percentages:
  # For threshold=0: 80 > 0, so forest cover should be 1.0 (100%)
  # For threshold=10: 80 > 10, so forest cover should be 1.0 (100%)
  # For threshold=70: 80 > 70, so forest cover should be 1.0 (100%) 
  # For threshold=90: 80 < 90, so forest cover should be 0.0 (0%)
  expect_length(result, 4)
  expect_equal(result[1], 1.0, tolerance = 1e-6)
  expect_equal(result[2], 1.0, tolerance = 1e-6)
  expect_equal(result[3], 1.0, tolerance = 1e-6)
  expect_equal(result[4], 0.0, tolerance = 1e-6)
  
  # Try with weighted_mean = TRUE for code coverage
  result_weighted <- sampleTreeCover(test_roi_sf, thresholds = c(70), forest_mask = test_raster, weighted_mean = TRUE)
  expect_equal(result_weighted[1], 1.0, tolerance = 1e-6)
})

test_that("terra::extract behavior is consistent with ID=FALSE parameter", {
  # Create a test raster with constant value of 80
  test_raster <- terra::rast(nrows=20, ncols=20, xmin=0, xmax=1, ymin=0, ymax=1, crs="EPSG:4326")
  terra::values(test_raster) <- rep(80, terra::ncell(test_raster))
  
  # Define a test region of interest
  test_roi <- sf::st_polygon(list(rbind(c(0.2, 0.2), c(0.25, 0.2), 
                                      c(0.25, 0.25), c(0.2, 0.25),
                                      c(0.2, 0.2))))
  test_roi_sf <- sf::st_sfc(test_roi, crs = 4326)
  test_roi_sf <- sf::st_sf(geometry = test_roi_sf)
  
  # Compare the extraction methods
  raw_extract <- terra::extract(test_raster, test_roi_sf)
  id_false_extract <- terra::extract(test_raster, test_roi_sf, ID = FALSE)
  
  # Original extraction should have ID column
  expect_true("ID" %in% colnames(raw_extract))
  expect_equal(raw_extract[[1]], 1) # ID value is 1
  expect_equal(raw_extract[[2]], 80) # Raster value is 80
  
  # ID=FALSE should have no ID column
  expect_false("ID" %in% colnames(id_false_extract))
  expect_equal(id_false_extract[[1]], 80) # First column is raster value
})

test_that("sampleAGBmap handles extraction correctly", {
  # Create a test AGB raster with constant value of 150
  test_agb_raster <- terra::rast(nrows=20, ncols=20, xmin=0, xmax=1, ymin=0, ymax=1, crs="EPSG:4326")
  terra::values(test_agb_raster) <- rep(150, terra::ncell(test_agb_raster))
  
  # Define a test region of interest
  test_roi <- sf::st_polygon(list(rbind(c(0.2, 0.2), c(0.25, 0.2), 
                                      c(0.25, 0.25), c(0.2, 0.25),
                                      c(0.2, 0.2))))
  test_roi_sf <- sf::st_sfc(test_roi, crs = 4326)
  
  # Test with custom AGB raster
  result_unweighted <- sampleAGBmap(test_roi_sf, weighted_mean = FALSE, agb_raster = test_agb_raster, dataset = "custom")
  expect_equal(result_unweighted, 150, tolerance = 1e-6)
  
  # Test with weighted mean
  result_weighted <- sampleAGBmap(test_roi_sf, weighted_mean = TRUE, agb_raster = test_agb_raster, dataset = "custom")
  expect_equal(result_weighted, 150, tolerance = 1e-6)
  
  # Test with some NA values
  test_agb_raster_with_na <- test_agb_raster
  terra::values(test_agb_raster_with_na)[1:100] <- NA
  result_with_na <- sampleAGBmap(test_roi_sf, weighted_mean = FALSE, agb_raster = test_agb_raster_with_na, dataset = "custom")
  expect_equal(result_with_na, 150, tolerance = 1e-6)
  
  # Test with zeros that should be converted to NA
  test_agb_raster_with_zeros <- test_agb_raster
  terra::values(test_agb_raster_with_zeros)[1:100] <- 0
  result_with_zeros <- sampleAGBmap(test_roi_sf, weighted_mean = FALSE, agb_raster = test_agb_raster_with_zeros, dataset = "custom")
  expect_equal(result_with_zeros, 150, tolerance = 1e-6)
})

# More complex tests that require downloading data are skipped by default
test_that("sampleAGBmap function behaves consistently - 1 tile esacci dataset", {
  skip("Skipping test that requires large dataset downloads")
  
  # Amazon Rainforest
  roi_amazon <- st_polygon(list(rbind(c(-62.2159, -3.4653), c(-62.2059, -3.4653),
                                   c(-62.2059, -3.4553), c(-62.2159, -3.4553),
                                   c(-62.2159, -3.4653))))
  roi_sf_amazon <- st_sfc(roi_amazon, crs = 4326)

  # Congo Rainforest
  roi_congo <- st_polygon(list(rbind(c(25.0089, 0.4735), c(25.0189, 0.4735),
                                  c(25.0189, 0.4835), c(25.0089, 0.4835),
                                  c(25.0089, 0.4735))))
  roi_sf_congo <- st_sfc(roi_congo, crs = 4326)

  # Test with both polygons
  result_amazon <- sampleAGBmap(roi_sf_amazon, dataset = "esacci")
  result_amazon_weighted_mean <- sampleAGBmap(roi_sf_amazon, weighted_mean = TRUE, dataset = "esacci")
  result_congo <- sampleAGBmap(roi_sf_congo, dataset = "esacci")
  result_congo_weighted_mean <- sampleAGBmap(roi_sf_congo, weighted_mean = TRUE, dataset = "esacci")

  expect_type(result_amazon, "double")
  expect_type(result_congo, "double")
  expect_type(result_amazon_weighted_mean, "double")
  expect_type(result_congo_weighted_mean, "double")
  expect_true(result_amazon != result_amazon_weighted_mean)
  expect_true(result_congo != result_congo_weighted_mean)
})

test_that("sampleAGBmap function behaves consistently - several tiles esacci dataset", {
  skip("Skipping test that requires large dataset downloads")
  
  # Amazon Rainforest
  roi_amazon <- st_polygon(list(rbind(c(-62.2159, -3.4653), c(-62.2059, -3.4653),
                                   c(-62.2059, -3.4553), c(-62.2159, -3.4553),
                                   c(-62.2159, -3.4653))))
  roi_sf_amazon <- st_sfc(roi_amazon, crs = 4326)
  roi_sf_amazon <- st_buffer(roi_sf_amazon, dist = 4)

  # Test with both polygons
  result_amazon <- sampleAGBmap(roi_sf_amazon, dataset = "esacci")

  expect_type(result_amazon, "double")
})

test_that("sampleTreeCover function behaves consistently", {
  skip("Skipping test that requires large dataset downloads")
  
  # Daintree Rainforest
  roi_daintree <- st_polygon(list(rbind(c(145.3833, -16.2500), c(145.3933, -16.2500),
                                     c(145.3933, -16.2400), c(145.3833, -16.2400),
                                     c(145.3833, -16.2500))))
  roi_sf_daintree <- st_sfc(roi_daintree, crs = 4326)

  # Sumatra Rainforest
  roi_sumatra <- st_polygon(list(rbind(c(101.3431, -0.5897), c(101.3531, -0.5897),
                                    c(101.3531, -0.5797), c(101.3431, -0.5797),
                                    c(101.3431, -0.5897))))
  roi_sf_sumatra <- st_sfc(roi_sumatra, crs = 4326)

  thresholds <- c(10, 30, 50)

  result_daintree <- sampleTreeCover(roi_sf_daintree, thresholds)
  result_sumatra <- sampleTreeCover(roi_sf_sumatra, thresholds)
  result_sumatra_weighted_mean <- sampleTreeCover(roi_sf_sumatra, thresholds, weighted_mean = TRUE)

  expect_type(result_daintree, "double")
  expect_type(result_sumatra, "double")
  expect_type(result_sumatra_weighted_mean, "double")
  expect_length(result_daintree, length(thresholds))
  expect_length(result_sumatra, length(thresholds))
  expect_length(result_sumatra_weighted_mean, length(thresholds))
})