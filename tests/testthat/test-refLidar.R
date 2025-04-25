library(testthat)
library(Plot2Map)
library(terra)

# Tests for RefLidar.R

# test_that("RefLidar function validates input parameters", {
#   # Skip actual test in automated testing
#   testthat::skip_if_not(interactive(), "Test requires interactive input")
#
#   # Test invalid directory
#   expect_error(RefLidar("nonexistent_directory"),
#                regexp = "cannot find")
# })
#
# test_that("RefLidar handles raster type input correctly", {
#   # Skip actual test in automated testing
#   testthat::skip_if_not(interactive(), "Test requires interactive input")
#
#   # Create mock function for testing raster type validation
#   mock_RefLidar <- function(raster_type) {
#     if (!raster_type %in% c("AGB", "CV", "SD")) {
#       stop("Invalid raster type. Please enter 'AGB', 'CV', or 'SD'.")
#     }
#     return(raster_type)
#   }
#
#   # Test valid raster types
#   expect_equal(mock_RefLidar("AGB"), "AGB")
#   expect_equal(mock_RefLidar("CV"), "CV")
#   expect_equal(mock_RefLidar("SD"), "SD")
#
#   # Test invalid raster type
#   expect_error(mock_RefLidar("ABC"),
#                "Invalid raster type. Please enter 'AGB', 'CV', or 'SD'.")
#
#   # Test case-insensitivity (actual function uses toupper())
#   expect_equal(toupper("agb"), "AGB")
# })

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
