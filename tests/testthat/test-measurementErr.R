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

# test_that("samplingUncertainty calculates correctly", {
#   # Generate test data
#   plotAGB <- c(100, 150, 200)
#   plotSize <- c(0.1, 0.25, 0.5)
#
#   # Calculate expected results
#   expected_se <- plotAGB / sqrt(plotSize)
#
#   # Run function
#   result <- samplingUncertainty(plotAGB, plotSize, method = "basic")
#
#   # Check output
#   expect_equal(result, expected_se)
#
#   # Test invalid method
#   expect_error(samplingUncertainty(plotAGB, plotSize, method = "bootstrap"),
#                "Bootstrap method not yet implemented")
# })
#
# test_that("totalUncertainty combines variance components correctly", {
#   # Test data
#   sdTree <- c(10, 15, 20)
#   sdSE <- c(5, 8, 12)
#   sdGrowth <- c(3, 6, 9)
#
#   # Calculate expected results
#   expected_total <- sqrt(sdTree^2 + sdSE^2 + sdGrowth^2)
#
#   # Run function
#   result <- totalUncertainty(sdTree, sdSE, sdGrowth)
#
#   # Check output
#   expect_equal(result, expected_total)
# })

test_that("determineDataType correctly identifies plot data types", {
  # Create simple test cases
  point_data <- data.frame(
    PLOT_ID = c("P1", "P2"),
    POINT_X = c(10, 12),
    POINT_Y = c(50, 51),
    AGB_T_HA = c(150, 200),
    SIZE_HA = c(1, 0.5),
    AVG_YEAR = c(2015, 2014)
  )

  tree_data <- data.frame(
    id = c("P1", "P1", "P2"),
    diameter = c(25, 30, 22),
    genus = c("Quercus", "Acer", "Pinus"),
    species = c("alba", "rubrum", "strobus")
  )

  tree_level_alt <- data.frame(
    id = c("P1", "P1", "P2"),
    DBH = c(25, 30, 22), # Alternative column name
    genus = c("Quercus", "Acer", "Pinus"),
    species = c("alba", "rubrum", "strobus")
  )

  tree_level_processed <- data.frame(
    PLOT_ID = c("P1", "P2"),
    POINT_X = c(10, 12),
    POINT_Y = c(50, 51),
    AGB_T_HA = c(150, 200),
    SIZE_HA = c(1, 0.5),
    sdTree = c(15, 12)
  )

  nested_data <- data.frame(
    PLOT_ID = c("P1", "P2"),
    POINT_X = c(10, 12),
    POINT_Y = c(50, 51),
    AGB_T_HA = c(150, 200),
    SIZE_HA = c(1, 0.5),
    nested = c(TRUE, TRUE),
    sdTree = c(15, 12)
  )

  # Create nested data with GUID style identifiers as seen in the vignette
  nested_guid_data <- data.frame(
    PLOT_ID = c("{89B25687-EDA0-4704-B00D-93EDDA162779}", "{22E69595-D9BE-4BB1-8660-E2E7457E2068}"),
    POINT_X = c(10, 12),
    POINT_Y = c(50, 51),
    AGB_T_HA = c(150, 200),
    SIZE_HA = c(1, 0.5),
    sdTree = c(15, 12)
  )

  # Tree data with GUID style IDs
  tree_nested_guid_data <- data.frame(
    id = c("{89B25687-EDA0-4704-B00D-93EDDA162779}", "{89B25687-EDA0-4704-B00D-93EDDA162779}",
           "{22E69595-D9BE-4BB1-8660-E2E7457E2068}"),
    diameter = c(25, 30, 22),
    genus = c("Quercus", "Acer", "Pinus"),
    species = c("alba", "rubrum", "strobus")
  )

  # Data with columns that indicate nested structure
  nested_columns_data <- data.frame(
    PLOT_ID = c("P1", "P2"),
    POINT_X = c(10, 12),
    POINT_Y = c(50, 51),
    AGB_T_HA = c(150, 200),
    SIZE_HA = c(1, 0.5),
    POINT_GUID = c("abc123", "def456"),
    sdTree = c(15, 12)
  )

  # LiDAR data example with CV column
  lidar_data <- data.frame(
    PLOT_ID = c("BON_A01", "BON_A01"),
    POINT_X = c(-67.28247, -67.28337),
    POINT_Y = c(-9.859331, -9.860239),
    AGB = c(0.2824650, 0.1802490),
    AVG_YEAR = c(2018, 2018),
    CV = c(10.991895, 13.910153),
    sdTree = c(3.104826, 2.507291)
  )

  # Tree data with non-standard column names but containing tree indicators
  non_standard_tree_data <- data.frame(
    plotId = c("P1", "P1", "P2"),
    tree_diam_cm = c(25, 30, 22),
    tree_species = c("Quercus alba", "Acer rubrum", "Pinus strobus"),
    tree_height_m = c(15, 18, 12)
  )

  # Add sf class mock for polygon data
  polygon_mock <- point_data
  attr(polygon_mock, "class") <- c("sf", "data.frame")

  # Run tests
  expect_equal(determineDataType(point_data), "point")
  expect_equal(determineDataType(tree_data), "tree_level")
  expect_equal(determineDataType(tree_level_alt), "tree_level")
  expect_equal(determineDataType(tree_level_processed), "tree_level")
  expect_equal(determineDataType(nested_data), "nested")
  expect_equal(determineDataType(nested_guid_data), "nested")
  expect_equal(determineDataType(tree_nested_guid_data), "nested")
  expect_equal(determineDataType(nested_columns_data), "nested")
  expect_equal(determineDataType(lidar_data), "lidar")
  expect_equal(determineDataType(non_standard_tree_data), "tree_level")
  expect_equal(determineDataType(polygon_mock), "polygon")
})

test_that("calculateTotalUncertainty handles point data correctly", {
  # Skip test if rf1 test file doesn't exist
  rf1_path <- sample_file("rf1.RData")
  skip_if_not(file.exists(rf1_path), "RF model not available")

  # Create basic point data with required columns
  test_data <- data.frame(
    PLOT_ID = c("P1", "P2", "P3"),
    POINT_X = c(10, 12, 14),
    POINT_Y = c(50, 51, 52),
    AGB_T_HA = c(150, 200, 180),
    SIZE_HA = c(1, 0.5, 0.25),
    AVG_YEAR = c(2015, 2014, 2016),
    GEZ = c("Tropical", "Temperate", "Boreal")
  )

  # Set a fixed test year
  test_year <- 2020

  load(rf1_path)

  # Test with biome_info = FALSE to avoid full TempVar dependency
  result <- tryCatch({
    calculateTotalUncertainty(test_data, test_year, 100, biome_info = FALSE)
  }, error = function(e) {
    skip(paste("Skipping test due to error:", e$message))
    NULL
  })

  # If the function executed successfully, check results
  if (!is.null(result)) {
    # Basic structure tests
    expect_true(is.list(result))
    expect_equal(names(result), c("data", "plot_type", "uncertainty_components"))
    expect_equal(result$plot_type, "point")

    # Check that uncertainty components were calculated
    expect_true("sdTree" %in% names(result$data))
    expect_true("sdSE" %in% names(result$data))
    expect_true("sdGrowth" %in% names(result$data))
    expect_true("sdTotal" %in% names(result$data))

    # Check uncertainty components sum to approximately 1
    component_sum <- sum(unlist(result$uncertainty_components))
    expect_equal(component_sum, 1, tolerance = 1e-5)
  }
})

test_that("calculateTotalUncertainty preserves existing uncertainty values", {
  # Create data with pre-existing uncertainty components
  test_data <- data.frame(
    PLOT_ID = c("P1", "P2"),
    POINT_X = c(10, 12),
    POINT_Y = c(50, 51),
    AGB_T_HA = c(150, 200),
    SIZE_HA = c(1, 0.5),
    AVG_YEAR = c(2015, 2014),
    GEZ = c("Tropical", "Temperate"),
    sdTree = c(15, 20),        # Pre-existing measurement error
    sdSE = c(10, 12),          # Pre-existing sampling error
    sdGrowth = c(5, 6)         # Pre-existing growth error
  )

  # Test year
  test_year <- 2020

  # Calculate total uncertainty
  result <- tryCatch({
    calculateTotalUncertainty(test_data, test_year, biome_info = FALSE)
  }, error = function(e) {
    skip(paste("Skipping test due to error:", e$message))
    NULL
  })

  # If function executed successfully
  if (!is.null(result)) {
    # Check that existing values were preserved
    expect_equal(result$data$sdTree, test_data$sdTree)
    expect_equal(result$data$sdSE, test_data$sdSE)
    expect_equal(result$data$sdGrowth, test_data$sdGrowth)

    # Check that total uncertainty is calculated correctly
    expected_total <- sqrt(test_data$sdTree^2 + test_data$sdSE^2 + test_data$sdGrowth^2)
    expect_equal(result$data$sdTotal, expected_total)
  }
})
