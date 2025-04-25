library(testthat)
library(Plot2Map)
library(dplyr)

# Tests for MeasurementErr.R

test_that("sd_tree calculates correct standard deviations", {

  # Load test data
  plotsTree <- utils::read.csv(sample_file("SampleTree.csv"), stringsAsFactors = FALSE)
  xyTree <- utils::read.csv(sample_file("SampleTreeXY.csv"), stringsAsFactors = FALSE)

  # Run function
  result <- sd_tree(plotsTree, xyTree, region = "World")

  # Check structure and column names
  expect_true(inherits(result, "data.frame"))
  expect_named(result, c('PLOT_ID', 'POINT_X', 'POINT_Y', 'SIZE_HA', 'AVG_YEAR', 'AGB_T_HA', 'sdTree'))

  # Check expected values
  expect_true(all(result$AGB_T_HA > 0))
  expect_true(all(result$sdTree >= 0))
  expect_true(all(result$SIZE_HA > 0))

  # # Test filtering (diameter >= 10 cm)
  # small_tree <- plotsTree[1, ]
  # small_tree$diameter <- 5  # Less than 10 cm
  # test_plot <- rbind(plotsTree[1:2, ], small_tree)
  # result_filtered <- sd_tree(test_plot, xyTree[1:3, ], region = "World")
  # expect_equal(nrow(unique(result_filtered)), nrow(unique(result_filtered$PLOT_ID)))
})

test_that("samplingUncertainty calculates correctly", {
  # Generate test data
  plotAGB <- c(100, 150, 200)
  plotSize <- c(0.1, 0.25, 0.5)

  # Calculate expected results
  expected_se <- plotAGB / sqrt(plotSize)

  # Run function
  result <- samplingUncertainty(plotAGB, plotSize, method = "basic")

  # Check output
  expect_equal(result, expected_se)

  # Test invalid method
  expect_error(samplingUncertainty(plotAGB, plotSize, method = "bootstrap"),
               "Bootstrap method not yet implemented")
})

test_that("totalUncertainty combines variance components correctly", {
  # Test data
  sdTree <- c(10, 15, 20)
  sdSE <- c(5, 8, 12)
  sdGrowth <- c(3, 6, 9)

  # Calculate expected results
  expected_total <- sqrt(sdTree^2 + sdSE^2 + sdGrowth^2)

  # Run function
  result <- totalUncertainty(sdTree, sdSE, sdGrowth)

  # Check output
  expect_equal(result, expected_total)
})
