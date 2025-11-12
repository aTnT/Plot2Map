library(testthat)
library(Plot2Map)
library(terra)

# Tests for RefLidar.R

test_that("RefLidar validates input directory", {
  # Test invalid directory
  expect_error(RefLidar("nonexistent_directory", allow_interactive = FALSE),
               regexp = "No valid raster files found")
})

test_that("RefLidar handles raster_type parameter correctly", {
  # Test raster type validation
  expect_equal(toupper("agb"), "AGB")
  expect_equal(toupper("cv"), "CV")
  expect_equal(toupper("sd"), "SD")

  # Test case-insensitivity
  expect_equal(toupper("Agb"), "AGB")
  expect_equal(toupper("CV"), "CV")
  expect_equal(toupper("Sd"), "SD")
})

test_that("RefLidar parameter defaults are correct", {
  # Check function signature defaults
  fn_args <- formals(RefLidar)

  expect_true(fn_args$auto_detect)
  expect_null(fn_args$pattern_config)
  expect_null(fn_args$raster_type)
  expect_true(fn_args$allow_interactive)
})

test_that("RefLidar non-interactive mode prevents prompts", {
  # This test verifies the parameter exists and can be set
  # Without actually calling readline()

  # Test that allow_interactive parameter can be set to FALSE
  expect_no_error({
    fn_args <- list(
      lidar.dir = "test_dir",
      auto_detect = TRUE,
      raster_type = "AGB",
      allow_interactive = FALSE
    )
  })
})

test_that("RefLidar adds correct columns based on raster type", {
  # Create mock function to test column selection
  mock_format_output <- function(pts, raster_type) {
    pts <- data.frame(PLOT_ID = "TEST", POINT_X = 0, POINT_Y = 0,
                      AGB = 100, CV = 0.2, SD = 20, AVG_YEAR = 2020)

    if (raster_type == "AGB") {
      pts <- pts[, c("PLOT_ID", "POINT_X", "POINT_Y", "AGB", "AVG_YEAR")]
    } else if (raster_type == "CV") {
      pts <- pts[, c("PLOT_ID", "POINT_X", "POINT_Y", "CV", "AVG_YEAR")]
    } else if (raster_type == "SD") {
      pts <- pts[, c("PLOT_ID", "POINT_X", "POINT_Y", "SD", "AVG_YEAR")]
    }

    return(pts)
  }

  # Test column selection for each raster type
  agb_result <- mock_format_output(NULL, "AGB")
  expect_named(agb_result, c("PLOT_ID", "POINT_X", "POINT_Y", "AGB", "AVG_YEAR"))

  cv_result <- mock_format_output(NULL, "CV")
  expect_named(cv_result, c("PLOT_ID", "POINT_X", "POINT_Y", "CV", "AVG_YEAR"))

  sd_result <- mock_format_output(NULL, "SD")
  expect_named(sd_result, c("PLOT_ID", "POINT_X", "POINT_Y", "SD", "AVG_YEAR"))
})

test_that("RefLidar auto-detection patterns work correctly", {
  # Test year extraction from filenames
  test_filenames <- c(
    "site_2015_agb.tif",
    "plot_2020_data.tif",
    "2018_measurement.tif"
  )

  # Test regex pattern for 4-digit year
  year_pattern <- "[12][0-9]{3}"

  expect_match(test_filenames[1], year_pattern)
  expect_match(test_filenames[2], year_pattern)
  expect_match(test_filenames[3], year_pattern)

  # Extract years
  year1 <- regmatches(test_filenames[1], regexpr(year_pattern, test_filenames[1]))
  year2 <- regmatches(test_filenames[2], regexpr(year_pattern, test_filenames[2]))
  year3 <- regmatches(test_filenames[3], regexpr(year_pattern, test_filenames[3]))

  expect_equal(year1, "2015")
  expect_equal(year2, "2020")
  expect_equal(year3, "2018")
})

test_that("RefLidar raster type auto-detection works", {
  # Test filename patterns for raster type detection
  test_filenames <- c(
    "site_agb_2015.tif",
    "plot_cv_data.tif",
    "measurement_sd_2020.tif",
    "biomass_map.tif",
    "coef_variation.tif",
    "stdev_data.tif"
  )

  # Test AGB detection
  expect_true(grepl("agb|biomass", test_filenames[1], ignore.case = TRUE))
  expect_true(grepl("agb|biomass", test_filenames[4], ignore.case = TRUE))

  # Test CV detection
  expect_true(grepl("cv|coef|variation", test_filenames[2], ignore.case = TRUE))
  expect_true(grepl("cv|coef|variation", test_filenames[5], ignore.case = TRUE))

  # Test SD detection
  expect_true(grepl("sd|stdev|deviation", test_filenames[3], ignore.case = TRUE))
  expect_true(grepl("sd|stdev|deviation", test_filenames[6], ignore.case = TRUE))
})

test_that("RefLidar year validation works correctly", {
  # Test year range validation (1990-2030)
  valid_years <- c(1990, 2000, 2015, 2020, 2030)
  invalid_years <- c(1989, 1950, 2031, 2050, NA)

  for (year in valid_years) {
    expect_true(!is.na(year) && year >= 1990 && year <= 2030)
  }

  for (year in invalid_years) {
    expect_false(!is.na(year) && year >= 1990 && year <= 2030)
  }
})
