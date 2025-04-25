library(testthat)
library(Plot2Map)
library(sf)

# Tests for Polygonize.R

test_that("Polygonize function produces consistent and correct results within the tolerance of 1e-2 for HA by design", {
  # Test case 1: Regular rectangular plot
  test_data1 <- data.frame(
    id = rep("plot1", 4),
    POINT_X = c(0, 1, 1, 0),
    POINT_Y = c(0, 0, 1, 1)
  )

  result1 <- Polygonize(test_data1, 4326)
  result1_sf <- st_as_sf(result1, coords = c("POINT_X", "POINT_Y"), crs = 4326)

  # Check output structure
  expect_true("data.frame" %in% class(result1))
  expect_true(all(c("PLOT_ID", "SIZE_HA", "POINT_X", "POINT_Y") %in% names(result1)))

  # Check geometry validity
  expect_true(all(st_is_valid(result1_sf)))

  # Check area calculation
  expect_equal(as.numeric(result1$SIZE_HA), 1236404, tolerance = 1e-2)

  # Test case 2: Irregular plot
  test_data2 <- data.frame(
    id = rep("plot2", 5),
    POINT_X = c(0, 1, 2, 1, 0),
    POINT_Y = c(0, 0, 1, 2, 1)
  )

  result2 <- Polygonize(test_data2, 4326)
  result2_sf <- st_as_sf(result2, coords = c("POINT_X", "POINT_Y"), crs = 4326)

  # Check output structure
  expect_true("data.frame" %in% class(result2))
  expect_true(all(c("PLOT_ID", "SIZE_HA", "POINT_X", "POINT_Y") %in% names(result2)))

  # Check geometry validity
  expect_true(all(st_is_valid(result2_sf)))

  # Check area calculation (approximate area of irregular pentagon)
  expect_equal(as.numeric(result2$SIZE_HA), 3090868, tolerance = 1e-2)

  # Test case 3: Multiple plots
  test_data3 <- data.frame(
    id = c(rep("plot1", 4), rep("plot2", 4)),
    POINT_X = c(0, 1, 1, 0, 2, 3, 3, 2),
    POINT_Y = c(0, 0, 1, 1, 2, 2, 3, 3)
  )

  result3 <- Polygonize(test_data3, 4326)
  result3_sf <- st_as_sf(result3, coords = c("POINT_X", "POINT_Y"), crs = 4326)

  # Check output structure
  expect_true("data.frame" %in% class(result3))
  expect_true(all(c("PLOT_ID", "SIZE_HA", "POINT_X", "POINT_Y") %in% names(result3)))

  # Check geometry validity
  expect_true(all(st_is_valid(result3_sf)))

  # Check number of plots
  expect_equal(nrow(result3), 2)

  # Check area calculation
  expect_equal(as.numeric(result3$SIZE_HA), c(1236404, 1235274), tolerance = 1e-2)

  # Check centroids
  expect_equal(result3$POINT_X, c(0.5, 2.5), tolerance = 1e-2)
  expect_equal(result3$POINT_Y, c(0.5, 2.5), tolerance = 1e-2)
})