library(testthat)
library(Plot2Map)

# Tests for Accuracy.R

test_that("Accuracy function test for intervals=8", {
  # Input dataframe where each bin has one value
  test_df <- data.frame(
    plotAGB_10 = c(25, 75, 125, 175, 225, 275, 350, 450),
    mapAGB = c(30, 70, 130, 180, 220, 280, 360, 460),
    sdPlot = rep(1, 8),
    sdMap = rep(1, 8)
  )

  # Expected output for intervals=8
  expected_df <- data.frame(
    `AGB bin (Mg/ha)` = c('0-50', '50-100', '100-150', '150-200', '200-250', '250-300', '300-400', '>400', 'total'),
    n = c(1, 1, 1, 1, 1, 1, 1, 1, 8),
    `AGBref (Mg/ha)` = c(25, 75, 125, 175, 225, 275, 350, 450, 213),
    `AGBmap (Mg/ha)` = c(30, 70, 130, 180, 220, 280, 360, 460, 216),
    RMSD = c(5, 5, 5, 5, 5, 5, 10, 10, 7),
    varPlot = c(1, 1, 1, 1, 1, 1, 1, 1, 1)
  )

  dir <- tempdir()

  refactored_output <- Accuracy(df = test_df, intervals = 8, dir = dir, str = 'test_refactored')

  expect_equal(refactored_output, expected_df, check.names = FALSE, tolerance = 1e-2)
})

test_that("Accuracy function test for intervals=7", {
  # Same input dataframe
  test_df <- data.frame(
    plotAGB_10 = c(25, 75, 125, 175, 225, 275, 350, 450),
    mapAGB = c(30, 70, 130, 180, 220, 280, 360, 460),
    sdPlot = rep(1, 8),
    sdMap = rep(1, 8)
  )

  # Expected output for intervals=7
  expected_df_7 <- data.frame(
    `AGB bin (Mg/ha)` = c('0-50', '50-100', '100-150', '150-200', '200-250', '250-300', '>300', 'total'),
    n = c(1, 1, 1, 1, 1, 1, 2, 8),
    `AGBref (Mg/ha)` = c(25, 75, 125, 175, 225, 275, 400, 212),
    `AGBmap (Mg/ha)` = c(30, 70, 130, 180, 220, 280, 410, 216),
    RMSD = c(5, 5, 5, 5, 5, 5, 10, 7),
    varPlot = c(1, 1, 1, 1, 1, 1, 1, 1)
  )

  dir <- tempdir()

  refactored_output_7 <- Accuracy(df = test_df, intervals = 7, dir = dir, str = 'test_refactored_7')

  expect_equal(refactored_output_7, expected_df_7, check.names = FALSE, tolerance = 1e-2)
})