library(testthat)
library(Plot2Map)

# Tests for example.R

test_that("sample_file returns correct file paths", {
  # Test with NULL parameter (should list all files)
  all_files <- sample_file()
  expect_type(all_files, "character")
  expect_gt(length(all_files), 0)
  
  # Test with specific file name
  sample_csv <- sample_file("SampleTree.csv")
  expect_type(sample_csv, "character")
  expect_true(grepl("SampleTree.csv$", sample_csv))
  expect_true(file.exists(sample_csv))
  
  # Test with non-existent file (should error)
  expect_error(sample_file("NonExistentFile.csv"), "Can't find package file.")
})

test_that("sample_lidar_folder returns correct folder paths", {
  # Test with valid folder
  lidar_folder <- sample_lidar_folder("SustainableLandscapeBrazil_v04/SLB_AGBmaps")
  expect_type(lidar_folder, "character")
  expect_equal(lidar_folder, "extdata/SustainableLandscapeBrazil_v04/SLB_AGBmaps")
  
  # The function should return the path even if it doesn't exist
  # (The path is relative to the package root)
  nonexistent_folder <- sample_lidar_folder("NonExistentFolder")
  expect_equal(nonexistent_folder, "extdata/NonExistentFolder")
})