library(testthat)
library(Plot2Map)

# Tests for TempVis.R

test_that("HistoTemp function behaves consistently", {
  skip("Test that generates files")
  
  test_data <- data.frame(
    AGB_T_HA = runif(100, 0, 600),
    AGB_T_HA_ORIG = runif(100, 0, 600)
  )
  
  # Check if function runs without errors
  expect_no_error(HistoTemp(test_data, 2020))
  
  # Check if output file is created
  outDir <- getwd()
  expect_true(file.exists(file.path(outDir, "histogram_tempfixed_2020.png")))
})

test_that("HistoShift function behaves consistently", {
  skip("Test that generates files")
  
  test_data <- data.frame(
    AGB_T_HA = runif(100, 0, 600),
    AGB_T_HA_ORIG = runif(100, 0, 600)
  )
  
  result <- HistoShift(test_data, 2020)
  
  # Check output structure
  expect_s3_class(result, "data.frame")
  expect_true(all(c("agb_Mgha_bins", "n_pre", "n_post", "agb_Mgha_pre", "agb_Mgha_post") %in% names(result)))
  
  # Check if output file is created
  outDir <- getwd()
  expect_true(file.exists(file.path(outDir, "TF_pre_post_change_2020.csv")))
})