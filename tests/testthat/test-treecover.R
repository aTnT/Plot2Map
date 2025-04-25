library(testthat)
library(Plot2Map)
library(sf)
library(terra)

# Tests for treecover.R

test_that("process_pair function works correctly", {
  skip("Skipping as this test requires downloading large datasets")
  
  # Create a temporary directory for test files
  temp_dir <- tempdir()
  
  id <- "GFC-2023-v1.11_10N_020E"
  year <- 2015
  treecover_threshold <- NULL
  output_folder <- file.path(temp_dir, "output")
  treecover_file <- "data/GFC/Hansen_GFC-2023-v1.11_treecover2000_10N_020E.tif"
  lossyear_file <- "data/GFC/Hansen_GFC-2023-v1.11_lossyear_10N_020E.tif"
  treecover_files <- c(treecover_file)
  lossyear_files <- c(lossyear_file)
  treecover_ids <- c("GFC-2023-v1.11_10N_020E")
  lossyear_ids <- c("GFC-2023-v1.11_10N_020E")
  baseline <- 2000

  # Test basic functionality
  result <- process_pair(id, year, treecover_threshold, output_folder,
                       treecover_files, lossyear_files, treecover_ids, lossyear_ids, baseline)

  expect_true(grepl("treecover_calc_2015_10N_020E.tif", result))
  expect_true(file.exists(file.path(output_folder, "Hansen_GFC-2023-v1.11_treecover_calc_2015_10N_020E.tif")))
  r_ras <- terra::rast(result)
  expect_s4_class(r_ras, "SpatRaster")

  # Test with treecover_threshold
  result_threshold <- process_pair(id, year, treecover_threshold = 50, output_folder,
                                 treecover_files, lossyear_files, treecover_ids, lossyear_ids, baseline)
  rt_ras <- terra::rast(result_threshold)
  expect_s4_class(rt_ras, "SpatRaster")

  # Test with baseline = 2010
  result_baseline_2010 <- process_pair(id, year, treecover_threshold = NULL, output_folder,
                                     treecover_files, lossyear_files, treecover_ids, lossyear_ids, baseline = 2010)
  expect_true(grepl("GLAD_TCC-2010_treecover_calc_2015_10N_020E.tif", result_baseline_2010))
  expect_true(file.exists(file.path(output_folder, "GLAD_TCC-2010_treecover_calc_2015_10N_020E.tif")))

  # Test with missing treecover or lossyear files (should throw an error)
  expect_error(
    process_pair(id, year, treecover_threshold = NULL, output_folder,
               treecover_files = c(), lossyear_files, treecover_ids, lossyear_ids, baseline)
  )
  expect_error(
    process_pair(id, year, treecover_threshold = NULL, output_folder,
               treecover_files, lossyear_files = c(), treecover_ids, lossyear_ids, baseline)
  )
})

# Test for compute_treecover function
test_that("compute_treecover function works correctly - 1 core", {
  skip("Skipping as this test requires downloading large datasets")
  
  year <- 2015
  gfc_folder <- "data/GFC"

  # Test with default parameters
  result <- compute_treecover(year, gfc_folder, coords = "10N_020E")
  expect_type(result, "list")
  expect_length(result, 1)
  r_ras <- terra::rast(result[[1]])
  expect_s4_class(r_ras, "SpatRaster")

  # Test with treecover_threshold
  result_threshold <- compute_treecover(year, gfc_folder, coords = "10N_020E", treecover_threshold = 50)
  expect_type(result_threshold, "list")
  expect_length(result_threshold, 1)
  rt_ras <- terra::rast(result_threshold[[1]])
  expect_s4_class(rt_ras, "SpatRaster")

  # Test with coords
  result_coords <- compute_treecover(year, gfc_folder, coords = "10N_020E")
  expect_type(result_coords, "list")
  expect_length(result_coords, 1)
  rc_ras <- terra::rast(result_coords[[1]])
  expect_s4_class(rc_ras, "SpatRaster")

  # Test with output_folder
  output_folder <- tempdir()
  result_output <- compute_treecover(year, gfc_folder, coords = "10N_020E", output_folder = output_folder)
  expect_type(result_output, "list")
  expect_length(result_output, 1)
  rof_ras <- terra::rast(result_output[[1]])
  expect_s4_class(rof_ras, "SpatRaster")
  expect_true(file.exists(file.path(output_folder, "GLAD_TCC-2010_treecover_calc_2015_10N_020E.tif")))

  # Test error for invalid year
  expect_error(compute_treecover(2000, gfc_folder), "Year must be between 2001 and 2023.")

  # Test error for non-existent coordinates
  expect_error(compute_treecover(year, gfc_folder, coords = "91N_999E"), "No matching files found for the specified coordinates.")
})