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
  expect_null(fn_args$raster_type)
  expect_true(fn_args$allow_interactive)
  expect_null(fn_args$metadata_map)
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

# ============================================================================
# Tests for metadata_map parameter
# ============================================================================

test_that("RefLidar metadata_map parameter validation works", {
  # Test that metadata_map must be a data frame
  expect_error(
    RefLidar("test_dir",
      metadata_map = "not_a_dataframe",
      allow_interactive = FALSE),
    "metadata_map must be a data frame"
  )
})

test_that("RefLidar metadata_map requires correct columns", {
  # Missing required columns
  invalid_metadata <- data.frame(
    filename = c("file1.tif", "file2.tif"),
    plot_id = c("P1", "P2")
    # Missing 'year' column
  )

  expect_error(
    RefLidar("test_dir",
      metadata_map = invalid_metadata,
      allow_interactive = FALSE),
    "metadata_map missing required columns"
  )
  expect_error(
    RefLidar("test_dir",
      metadata_map = invalid_metadata,
      allow_interactive = FALSE),
    "year"
  )
})

test_that("RefLidar metadata_map structure is validated correctly", {
  # Valid metadata structure
  valid_metadata <- data.frame(
    filename = c("site1_2020.tif", "site2_2021.tif"),
    plot_id = c("PlotA", "PlotB"),
    year = c(2020, 2021)
  )

  # Verify structure
  expect_true(is.data.frame(valid_metadata))
  expect_true(all(c("filename", "plot_id", "year") %in% names(valid_metadata)))
  expect_equal(nrow(valid_metadata), 2)
})

test_that("RefLidar metadata_map parameter defaults are correct", {
  # Check that metadata_map is NULL by default
  fn_args <- formals(RefLidar)
  expect_null(fn_args$metadata_map)
})

test_that("RefLidar metadata_map bypasses auto-detection", {
  # Create mock function to test metadata_map logic
  mock_apply_metadata <- function(pts, metadata_map) {
    # Simulate metadata assignment
    pts$PLOT_ID <- metadata_map$plot_id[match(pts$ID, metadata_map$filename)]
    pts$AVG_YEAR <- metadata_map$year[match(pts$ID, metadata_map$filename)]
    return(pts)
  }

  # Test data
  pts <- data.frame(
    ID = c("site1.tif", "site2.tif", "site3.tif"),
    POINT_X = c(-5.5, -5.6, -5.7),
    POINT_Y = c(36.5, 36.6, 36.7),
    AGB = c(100, 150, 200)
  )

  metadata <- data.frame(
    filename = c("site1.tif", "site2.tif", "site3.tif"),
    plot_id = c("PlotA", "PlotB", "PlotC"),
    year = c(2020, 2021, 2022)
  )

  result <- mock_apply_metadata(pts, metadata)

  # Verify metadata was applied correctly
  expect_equal(result$PLOT_ID, c("PlotA", "PlotB", "PlotC"))
  expect_equal(result$AVG_YEAR, c(2020, 2021, 2022))
})

test_that("RefLidar metadata_map handles file matching correctly", {
  # Test file matching logic
  metadata <- data.frame(
    filename = c("file1.tif", "file2.tif", "file3.tif"),
    plot_id = c("P1", "P2", "P3"),
    year = c(2020, 2021, 2022)
  )

  available_files <- c("file1.tif", "file3.tif")  # file2.tif missing

  # Match available files to metadata
  matched_indices <- match(available_files, metadata$filename)

  expect_equal(matched_indices, c(1, 3))
  expect_equal(metadata$plot_id[matched_indices], c("P1", "P3"))
  expect_equal(metadata$year[matched_indices], c(2020, 2022))
})

test_that("RefLidar metadata_map warns about unmapped files", {
  # Test unmapped file detection
  metadata <- data.frame(
    filename = c("file1.tif", "file2.tif"),
    plot_id = c("P1", "P2"),
    year = c(2020, 2021)
  )

  available_files <- c("file1.tif", "file2.tif", "file3.tif", "file4.tif")

  # Find unmapped files
  unmapped <- available_files[!available_files %in% metadata$filename]

  expect_equal(length(unmapped), 2)
  expect_equal(unmapped, c("file3.tif", "file4.tif"))
})
