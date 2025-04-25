library(testthat)
library(Plot2Map)
library(sf)
library(terra)

# Tests for BlockMeans.R

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

test_that("sampleAGBmap function behaves consistently - 1 tile gedi dataset", {
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
  result_amazon <- sampleAGBmap(roi_sf_amazon, dataset = "gedi")
  result_amazon_weighted_mean <- sampleAGBmap(roi_sf_amazon, weighted_mean = TRUE, dataset = "gedi", gedi_l4b_band = "MU")
  result_congo <- sampleAGBmap(roi_sf_congo, dataset = "gedi")
  result_congo_weighted_mean <- sampleAGBmap(roi_sf_congo, weighted_mean = TRUE, dataset = "gedi", gedi_l4b_band = "MU")

  expect_type(result_amazon, "double")
  expect_type(result_congo, "double")
  expect_type(result_amazon_weighted_mean, "double")
  expect_type(result_congo_weighted_mean, "double")
  expect_true(result_amazon != result_amazon_weighted_mean)
  expect_true(result_congo != result_congo_weighted_mean)
})

test_that("sampleAGBmap function behaves consistently - 1 tile gedi dataset, several resolution", {
  skip("Skipping test that requires large dataset downloads")
  
  # Amazon Rainforest
  roi_amazon <- st_polygon(list(rbind(c(-62.2159, -3.4653), c(-62.2059, -3.4653),
                                     c(-62.2059, -3.4553), c(-62.2159, -3.4553),
                                     c(-62.2159, -3.4653))))
  roi_sf_amazon <- st_sfc(roi_amazon, crs = 4326)

  # Test with both polygons
  result_amazon_1mdeg <- sampleAGBmap(roi_sf_amazon, dataset = "gedi", gedi_l4b_resolution = 0.001)
  result_amazon_10mdeg <- sampleAGBmap(roi_sf_amazon, dataset = "gedi", gedi_l4b_resolution = 0.01)

  expect_type(result_amazon_1mdeg, "double")
  expect_type(result_amazon_10mdeg, "double")
  expect_true(result_amazon_1mdeg != result_amazon_10mdeg)
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