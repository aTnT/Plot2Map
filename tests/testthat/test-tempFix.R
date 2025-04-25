library(testthat)
library(Plot2Map)

# Tests for TempFix.R

# Test internal consistency
test_that("TempApply function behaves consistently", {
  set.seed(123)
  test_data <- plots[sample(nrow(plots), 10), ]
  test_data <- BiomePair(test_data)

  result <- TempApply(test_data, 2004)

  # Check output structure
  expect_s3_class(result, "data.frame")
  expect_true(all(c("AGB_T_HA", "AGB_T_HA_ORIG") %in% names(result)))

  # Check for expected adjustments
  expect_true(all(result$AGB_T_HA[result$AVG_YEAR < 2004] >= result$AGB_T_HA_ORIG[result$AVG_YEAR < 2004]))
  expect_true(all(result$AGB_T_HA[result$AVG_YEAR > 2004] <= result$AGB_T_HA_ORIG[result$AVG_YEAR > 2004]))
})

test_that("TempVar function behaves consistently", {
  set.seed(1234)
  test_data <- plots[sample(nrow(plots), 10), ]
  test_data <- BiomePair(test_data)

  result <- TempVar(test_data, 2005)

  # Check output structure
  expect_s3_class(result, "data.frame")
  expect_true(all(c("AGB_T_HA", "sdGrowth") %in% names(result)))

  # Check for expected calculations
  expect_true(all(result$sdGrowth >= 0))
  expect_false(any(is.na(result$sdGrowth)))
})