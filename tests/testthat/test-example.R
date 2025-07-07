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

})


test_that("Test with non-existent file (should error)", {
  skip_on_ci()
  # Test with non-existent file (should error)
  # Use regex to match either "Can't find package file." or "no file found"
  expect_error(sample_file("NonExistentFile.csv"), "(Can't find package file|no file found)")
})


test_that("sample_lidar_folder returns correct folder paths", {
  # Test with valid folder
  lidar_folder <- sample_lidar_folder("SustainableLandscapeBrazil_v04/SLB_AGBmaps")
  expect_type(lidar_folder, "character")
  # Now expect an absolute path that contains the directory
  expect_true(grepl("SustainableLandscapeBrazil_v04/SLB_AGBmaps$", lidar_folder))
  expect_true(dir.exists(lidar_folder))
  
  # Test with non-existent folder (should error)
  # Use regex to match error message
  expect_error(sample_lidar_folder("NonExistentFolder"), "Can't find package file")
})
