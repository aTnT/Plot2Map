library(testthat)
library(Plot2Map)
library(sf)

# Tests for helper.R

test_that("sf_to_sf_with_coords adds coordinate columns", {
  # Skip test if sf package not available
  skip_if_not_installed("sf")

  # Create sample sf object - fix how we create the sf object
  points <- data.frame(
    PLOT_ID = c("A1", "B2"),
    AGB_T_HA = c(100, 150),
    geometry = c(
      "POINT(10.5 51.3)",
      "POINT(11.2 51.5)"
    )
  )
  points_sf <- sf::st_as_sf(points, wkt = "geometry", crs = 4326)

  # Apply function
  result <- sf_to_sf_with_coords(points_sf)

  # Test result
  expect_true(inherits(result, "sf"))
  expect_named(result, c("PLOT_ID", "AGB_T_HA", "geometry", "POINT_X", "POINT_Y"), ignore.order = TRUE)
  expect_equal(result$POINT_X, c(10.5, 11.2))
  expect_equal(result$POINT_Y, c(51.3, 51.5))

  # Test error for non-sf input
  expect_error(sf_to_sf_with_coords(as.data.frame(points)), "Input must be an sf object")
})

test_that("sf_to_df_with_coords converts sf to dataframe with coords", {
  # Skip test if sf package not available
  skip_if_not_installed("sf")

  # Create sample sf object - fix how we create the sf object
  points <- data.frame(
    PLOT_ID = c("A1", "B2"),
    AGB_T_HA = c(100, 150),
    geometry = c(
      "POINT(10.5 51.3)",
      "POINT(11.2 51.5)"
    )
  )
  points_sf <- sf::st_as_sf(points, wkt = "geometry", crs = 4326)

  # Apply function
  result <- sf_to_df_with_coords(points_sf)

  # Test result
  expect_true(is.data.frame(result))
  expect_false(inherits(result, "sf"))
  expect_named(result, c("PLOT_ID", "AGB_T_HA", "POINT_X", "POINT_Y"), ignore.order = TRUE)
  expect_equal(result$POINT_X, c(10.5, 11.2))
  expect_equal(result$POINT_Y, c(51.3, 51.5))

  # Test CRS transformation
  utm_points <- st_transform(points_sf, 32632)  # UTM zone 32N
  utm_result <- sf_to_df_with_coords(utm_points)
  expect_equal(utm_result$POINT_X, c(10.5, 11.2), tolerance = 0.001)
  expect_equal(utm_result$POINT_Y, c(51.3, 51.5), tolerance = 0.001)

  # Test error for non-sf input
  expect_error(sf_to_df_with_coords(as.data.frame(points)), "Input must be an sf object")
})

test_that("check_and_convert_plt properly validates and converts inputs", {

  # Create sample sf object - fix how we create the sf object
  points <- data.frame(
    PLOT_ID = c("A1", "B2"),
    AGB_T_HA = c(100, 150),
    geometry = c(
      "POINT(10.5 51.3)",
      "POINT(11.2 51.5)"
    )
  )
  points_sf <- sf::st_as_sf(points, wkt = "geometry", crs = 4326)

  # Create sample dataframe
  points_df <- data.frame(
    PLOT_ID = c("A1", "B2"),
    AGB_T_HA = c(100, 150),
    POINT_X = c(10.5, 11.2),
    POINT_Y = c(51.3, 51.5)
  )

  # Apply function to sf object
  result_sf <- check_and_convert_plt(points_sf)

  # Test sf result
  expect_true(is.data.frame(result_sf))
  expect_false(inherits(result_sf, "sf"))
  expect_named(result_sf, c("PLOT_ID", "AGB_T_HA", "POINT_X", "POINT_Y"), ignore.order = TRUE)

  # Apply function to dataframe
  result_df <- check_and_convert_plt(points_df)

  # Test dataframe result
  expect_true(is.data.frame(result_df))
  expect_named(result_df, c("PLOT_ID", "AGB_T_HA", "POINT_X", "POINT_Y"), ignore.order = TRUE)

  # Test error for invalid input (missing coordinate columns)
  invalid_df <- data.frame(PLOT_ID = c("A1", "B2"), AGB_T_HA = c(100, 150))
  expect_error(check_and_convert_plt(invalid_df), "must contain 'POINT_X' and 'POINT_Y'")

  # Test with ez=TRUE (would check for presence of ecozone columns)
  points_df_ez <- points_df
  points_df_ez$ZONE <- c(1, 2)
  points_df_ez$FAO.ecozone <- c("Trop", "Temp")
  points_df_ez$GEZ <- c("TAr", "TeBF")

  result_df_ez <- check_and_convert_plt(points_df_ez, ez = TRUE)
  expect_true(is.data.frame(result_df_ez))

  # Test error when ez=TRUE but ecozone columns missing
  expect_error(check_and_convert_plt(points_df, ez = TRUE),
               "must contain 'ZONE', 'FAO.ecozone', 'GEZ'")
})
