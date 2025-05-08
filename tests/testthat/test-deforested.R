library(testthat)
library(Plot2Map)
library(sf)
library(terra)

# Tests for Deforested.R

test_that("terra::extract behavior in Deforested.R is consistent with ID=FALSE parameter", {
  skip("Skipping this test as it requires mocking external functions")
  
  # Create a test forest loss raster with values 0 (no loss) and 5 (loss in 2005)
  test_raster <- terra::rast(nrows=20, ncols=20, xmin=0, xmax=1, ymin=0, ymax=1, crs="EPSG:4326")
  terra::values(test_raster) <- rep(c(0, 5), length.out=terra::ncell(test_raster))
  
  # Create a test directory and save the raster
  test_dir <- tempfile()
  dir.create(test_dir, recursive = TRUE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)
  
  test_file <- file.path(test_dir, "Hansen_GFC-2023-v1.11_lossyear_10N_000E.tif")
  terra::writeRaster(test_raster, test_file, overwrite=TRUE)
  
  # Create a test plot
  test_plot <- data.frame(
    POINT_X = 0.5,
    POINT_Y = 0.5,
    PLOT_ID = 1,
    SIZE_HA = 1
  )
  test_plot <- sf::st_as_sf(test_plot, coords = c("POINT_X", "POINT_Y"), crs = 4326)
  
  # The test needs mocking of gfcanalysis functions which is not currently supported
})

# Test using ACTUAL raster extract behavior to verify ID=FALSE implementation
test_that("terra::extract with ID=FALSE returns correct values", {
  # Create a test forest loss raster with values 0 (no loss) and 5 (loss in 2005)
  test_raster <- terra::rast(nrows=5, ncols=5, xmin=0, xmax=1, ymin=0, ymax=1, crs="EPSG:4326")
  terra::values(test_raster) <- rep(5, terra::ncell(test_raster))
  
  # Create a test point (using point to get single value rather than multiple polygon cells)
  test_point <- sf::st_point(c(0.5, 0.5))
  test_point_sf <- sf::st_sfc(test_point, crs = 4326)
  test_point_sf <- sf::st_sf(geometry = test_point_sf)
  
  # Extract values using the original way (with [[2]] to get values)
  original_extract <- terra::extract(test_raster, test_point_sf)[[2]]
  
  # Extract values using the new way (with ID=FALSE and [[1]])
  new_extract <- terra::extract(test_raster, test_point_sf, ID=FALSE)[[1]]
  
  # Both should return the same result
  expect_equal(original_extract, new_extract)
  expect_equal(new_extract, 5)
})

test_that("Deforested function behaves consistently across map_years", {
  skip("Skipping because this test requires large datasets")
  
  # Test params
  #plots <- read.csv(file.path("data", "SamplePlots.csv"))
  set.seed(42)
  plots_sample <- c(sample(1:dim(plots)[1], 10), 182, 200, 323, 6765) # handpicking some plots with deforestation
  sampled_plots <- plots[plots_sample,]

  result_2010 <- Deforested(sampled_plots, 2010)
  result_2020 <- Deforested(sampled_plots, 2020)
  result_2023 <- Deforested(sampled_plots, 2023)

  # Compare results
  expect_equal(
    unique(result_2010$all_plots |>
             dplyr::filter(defo > 0) |>
             dplyr::select(defo_start_year)),
    unique(result_2020$all_plots |>
             dplyr::filter(defo > 0) |>
             dplyr::select(defo_start_year))
  )
  expect_equal(
    unique(result_2020$all_plots |>
             dplyr::filter(defo > 0) |>
             dplyr::select(defo_start_year)),
    unique(result_2023$all_plots |>
             dplyr::filter(defo > 0) |>
             dplyr::select(defo_start_year))
  )
})

test_that("Deforested function behaves consistently across gfc_dataset_year", {
  skip("Skipping because this test requires large datasets")
  
  # Test params
  #plots <- read.csv(file.path("data", "SamplePlots.csv"))
  set.seed(42)
  plots_sample <- c(sample(1:dim(plots)[1], 10), 182, 200, 323, 6765) # handpicking some plots with deforestation
  sampled_plots <- plots[plots_sample,]

  result_gfc_2015 <- Deforested(sampled_plots, map_year = 2010, gfc_dataset_year = 2015)
  result_gfc_2019 <- Deforested(sampled_plots, map_year = 2010, gfc_dataset_year = 2019)
  result_gfc_2022 <- Deforested(sampled_plots, map_year = 2010, gfc_dataset_year = 2022)
  result_gfc_2023 <- Deforested(sampled_plots, map_year = 2010, gfc_dataset_year = 2023)
  result_gfc_latest <- Deforested(sampled_plots, map_year = 2010)

  # Compare results
  expect_equal(result_gfc_2015$non_deforested_plots, result_gfc_2019$non_deforested_plots)
  expect_equal(result_gfc_2022$non_deforested_plots, result_gfc_2019$non_deforested_plots)
  expect_equal(result_gfc_2015$non_deforested_plots, result_gfc_2023$non_deforested_plots)
  expect_equal(result_gfc_latest$non_deforested_plots, result_gfc_2023$non_deforested_plots)

  expect_equal(result_gfc_latest$all_plots, result_gfc_2023$all_plots)

  expect_equal(sum(result_gfc_2015$all_plots$defo), 2)
  expect_equal(sum(result_gfc_2019$all_plots$defo), 4)
  expect_equal(sum(result_gfc_2022$all_plots$defo), 5)
  expect_equal(sum(result_gfc_2023$all_plots$defo), sum(result_gfc_latest$all_plots$defo))
})

test_that("Deforested function behaves consistently", {
  skip("Skipping because this test requires access to plots data")
  
  # Test params
  #plots <- read.csv(file.path("data", "SamplePlots.csv"))
  set.seed(42)
  plots_sample <- sample(1:dim(plots)[1], 2)
  sampled_plots <- plots[plots_sample,]

  result <- Deforested(sampled_plots, map_year = 2010)

  # Check output structure
  expect_type(result, "list")
  expect_length(result, 2)
  expect_s3_class(result[[1]], "data.frame")
  expect_s3_class(result[[2]], "data.frame")

  # Check column names
  expected_cols <- c("PLOT_ID", "AGB_T_HA", "AVG_YEAR", "geometry", "defo")
  expect_true(all(expected_cols %in% colnames(result[[2]])))

  # Check deforestation filtering
  expect_true(all(result[[2]]$defo <= 0.05))
})