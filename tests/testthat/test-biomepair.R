library(testthat)
library(Plot2Map)
library(terra)

# Tests for BiomePair.R

# test_that("Old and new BiomePair functions produce consistent results", {
#   # Create sample data
#   test_data <- data.frame(
#     POINT_X = c(-3.007, 145.627, 24.539),
#     POINT_Y = c(6.010, -37.592, 0.993)
#   )
#
#   old_result <- old_BiomePair(test_data)
#   new_result <- BiomePair(test_data)
#
#   expect_equal(old_result$ZONE, new_result$ZONE)
#   expect_equal(old_result$FAO.ecozone, new_result$FAO.ecozone)
#   expect_equal(old_result$GEZ, new_result$GEZ)
# })

test_that("BiomePair function behaves consistently", {
  test_data <- data.frame(
    POINT_X = c(-3.007, 145.627, 24.539),
    POINT_Y = c(6.010, -37.592, 0.993)
  )

  result <- BiomePair(test_data)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("ZONE", "FAO.ecozone", "GEZ") %in% names(result)))

  expect_true(all(result$ZONE %in% c("Africa", "Australia", "S.America", "C.America", "Asia", "Europe")))
  expect_false(any(result$GEZ == "Water"))
  expect_false(any(result$GEZ == "Polar"))

  expect_true(all(sapply(seq_len(nrow(result)), function(i)
    grepl(result$GEZ[i], result$FAO.ecozone[i], ignore.case = TRUE))))
})
