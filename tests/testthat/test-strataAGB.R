library(testthat)
library(Plot2Map)

# Tests for StrataAGB.R

# Tests for assign_strata_weights
test_that("assign_strata_weights assigns weights correctly", {
  # Test 1: Default weights
  plots <- data.frame(stratum = c("A", "B", "C"))
  result <- assign_strata_weights(plots)
  expect_equal(result$wt, c(0.22, 0.22, 0.68), tolerance = 1e-8)

  # Test 2: Custom weights
  custom_weights <- c("Low" = 0.3, "High" = 0.7)
  plots <- data.frame(stratum = c("Low", "High"))
  result <- assign_strata_weights(plots, strata_weights = custom_weights)
  expect_equal(result$wt, c(0.3, 0.7), tolerance = 1e-8)

  # Test 3: Error when stratum column is missing
  plots <- data.frame(value = 1:3)
  expect_error(assign_strata_weights(plots), "The 'plots' data frame must contain a 'stratum' column")

  # Test 4: Error when strata don't match weights
  plots <- data.frame(stratum = c("A", "D"))
  expect_error(assign_strata_weights(plots), "All strata in 'plots\\$stratum' must match names in 'strata_weights'")
})

# Tests for StrataAGB
test_that("StrataAGB computes weighted AGB and SD correctly", {
  # Test 1: Default weights with stratum column
  plots <- data.frame(
    AGB_T_HA = c(100, 150, 200),
    sdTree = c(10, 15, 20),
    stratum = c("A", "B", "C")
  )
  result <- StrataAGB(plots, verbose = FALSE)
  expected_wm <- (100 * 0.22 + 150 * 0.22 + 200 * 0.68) / (0.22 + 0.22 + 0.68)
  expected_wsd <- (10 * 0.22 + 15 * 0.22 + 20 * 0.68) / (0.22 + 0.22 + 0.68)
  expect_equal(result$wm, expected_wm, tolerance = 1e-8)
  expect_equal(result$wsd, expected_wsd, tolerance = 1e-8)
  expect_equal(result$weighted_plots$wt, c(0.22, 0.22, 0.68), tolerance = 1e-8)

  # Test 2: Default weights without stratum column (5 rows)
  plots <- data.frame(
    AGB_T_HA = c(100, 150, 200, 180, 170),
    sdTree = c(10, 15, 20, 18, 17)
  )
  result <- StrataAGB(plots, verbose = FALSE)
  expected_wm <- (100 * 0.22 + 150 * 0.22 + 200 * 0.68 + 180 * 0.68 + 170 * 0.68) / (0.22 + 0.22 + 0.68 * 3)
  expected_wsd <- (10 * 0.22 + 15 * 0.22 + 20 * 0.68 + 18 * 0.68 + 17 * 0.68) / (0.22 + 0.22 + 0.68 * 3)
  expect_equal(result$wm, expected_wm, tolerance = 1e-8)
  expect_equal(result$wsd, expected_wsd, tolerance = 1e-8)
  expect_equal(result$weighted_plots$wt, c(0.22, 0.22, 0.68, 0.68, 0.68), tolerance = 1e-8)

  # Test 3: Custom weights
  custom_weights <- c("Low" = 0.3, "High" = 0.7)
  plots <- data.frame(
    AGB_T_HA = c(120, 180),
    sdTree = c(12, 18),
    stratum = c("Low", "High")
  )
  result <- StrataAGB(plots, strata_weights = custom_weights, verbose = FALSE)
  expected_wm <- (120 * 0.3 + 180 * 0.7) / (0.3 + 0.7)
  expected_wsd <- (12 * 0.3 + 18 * 0.7) / (0.3 + 0.7)
  expect_equal(result$wm, expected_wm, tolerance = 1e-8)
  expect_equal(result$wsd, expected_wsd, tolerance = 1e-8)

  # Test 4: Error when required columns are missing
  plots <- data.frame(AGB_T_HA = 1:3)
  expect_error(StrataAGB(plots), "The 'plots' data frame must contain 'AGB_T_HA' and 'sdTree' columns")

  # Test 5: Error when AGB_T_HA or sdTree is non-numeric
  plots <- data.frame(
    AGB_T_HA = c("100", "150"),
    sdTree = c(10, 15),
    stratum = c("A", "B")
  )
  expect_error(StrataAGB(plots), "'AGB_T_HA' and 'sdTree' must be numeric")

  # Test 6: Error when weights are invalid
  plots <- data.frame(
    AGB_T_HA = c(100, 150),
    sdTree = c(10, 15),
    stratum = c("A", "B")
  )
  expect_error(StrataAGB(plots, strata_weights = c("A" = -0.1, "B" = 0.1)),
               "'strata_weights' must be non-negative and sum to a positive value")

  # Test 7: Error when row count doesn't match default weights without stratum
  plots <- data.frame(
    AGB_T_HA = c(100, 150),
    sdTree = c(10, 15)
  )
  expect_error(StrataAGB(plots), "Without 'stratum', 'plots' must have exactly 5 rows to match default weights")
})