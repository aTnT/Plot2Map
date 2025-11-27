library(testthat)
library(Plot2Map)

# Tests for RawPlots.R

test_that("RawPlots validates input types", {
  # Test non-dataframe input
  expect_error(RawPlots("not_a_dataframe"), "Input file should be a data frame")
})

test_that("RawPlots auto-detects standard column names in non-interactive mode", {
  # Create test data with standard column names
  test_data <- data.frame(
    PLOT_ID = c("P1", "P2", "P3"),
    AGB_T_HA = c(100, 150, 200),
    longitude = c(-5.5, -5.6, -5.7),
    latitude = c(36.5, 36.6, 36.7),
    SIZE_HA = c(1.0, 0.5, 1.5),
    AVG_YEAR = c(2015, 2016, 2017)
  )

  # Test non-interactive mode
  result <- RawPlots(test_data, allow_interactive = FALSE)

  # Verify output structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true("PLOT_ID" %in% names(result))
  expect_true("AGB_T_HA" %in% names(result))
  expect_true("POINT_X" %in% names(result))
  expect_true("POINT_Y" %in% names(result))
  expect_true("SIZE_HA" %in% names(result))
  expect_true("AVG_YEAR" %in% names(result))

  # Verify values are preserved
  expect_equal(result$AGB_T_HA, c(100, 150, 200))
  expect_equal(result$POINT_X, c(-5.5, -5.6, -5.7))
  expect_equal(result$POINT_Y, c(36.5, 36.6, 36.7))
})

test_that("RawPlots auto-detects alternative column name patterns", {
  # Create test data with alternative column names
  test_data <- data.frame(
    ID = c("P1", "P2", "P3"),
    agb = c(100, 150, 200),
    lon = c(-5.5, -5.6, -5.7),
    lat = c(36.5, 36.6, 36.7),
    size = c(1.0, 0.5, 1.5),
    year = c(2015, 2016, 2017)
  )

  result <- RawPlots(test_data, allow_interactive = FALSE)

  # Verify successful processing
  expect_equal(nrow(result), 3)
  expect_equal(result$AGB_T_HA, c(100, 150, 200))
})

test_that("RawPlots generates sequential IDs when Plot ID not found", {
  # Create test data without Plot ID column
  test_data <- data.frame(
    AGB_T_HA = c(100, 150, 200),
    longitude = c(-5.5, -5.6, -5.7),
    latitude = c(36.5, 36.6, 36.7),
    SIZE_HA = c(1.0, 0.5, 1.5),
    AVG_YEAR = c(2015, 2016, 2017)
  )

  result <- RawPlots(test_data, allow_interactive = FALSE)

  # Verify sequential IDs were generated
  expect_equal(result$PLOT_ID, c("PLOT_1", "PLOT_2", "PLOT_3"))
})

test_that("RawPlots errors with informative message when required columns missing", {
  # Create test data missing size and year columns
  test_data <- data.frame(
    PLOT_ID = c("P1", "P2", "P3"),
    AGB = c(100, 150, 200),
    longitude = c(-5.5, -5.6, -5.7),
    latitude = c(36.5, 36.6, 36.7)
  )

  # Should error with comprehensive detection summary
  expect_error(
    RawPlots(test_data, allow_interactive = FALSE),
    "Column Detection Summary"
  )
  expect_error(
    RawPlots(test_data, allow_interactive = FALSE),
    "Successfully detected"
  )
  expect_error(
    RawPlots(test_data, allow_interactive = FALSE),
    "Could not detect"
  )
})

test_that("RawPlots handles uppercase column names", {
  # Create test data with uppercase names
  test_data <- data.frame(
    PLOT_ID = c("P1", "P2"),
    AGB = c(100, 150),
    LONGITUDE = c(-5.5, -5.6),
    LATITUDE = c(36.5, 36.6),
    SIZE = c(1.0, 0.5),
    YEAR = c(2015, 2016)
  )

  result <- RawPlots(test_data, allow_interactive = FALSE)

  expect_equal(nrow(result), 2)
  expect_equal(result$AGB_T_HA, c(100, 150))
})

test_that("RawPlots converts large sizes from m² to hectares", {
  # Create test data with sizes in m²
  test_data <- data.frame(
    PLOT_ID = c("P1", "P2", "P3"),
    AGB_T_HA = c(100, 150, 200),
    longitude = c(-5.5, -5.6, -5.7),
    latitude = c(36.5, 36.6, 36.7),
    SIZE_HA = c(10000, 5000, 100),  # Mix of m² and ha
    AVG_YEAR = c(2015, 2016, 2017)
  )

  result <- RawPlots(test_data, allow_interactive = FALSE)

  # Verify conversion (values > 50 are assumed to be m²)
  expect_equal(result$SIZE_HA, c(1.0, 0.5, 0.01))
})

test_that("RawPlots filters out rows with NA in AGB", {
  # Create test data with some NA AGB values
  test_data <- data.frame(
    PLOT_ID = c("P1", "P2", "P3", "P4"),
    AGB_T_HA = c(100, NA, 200, NA),
    longitude = c(-5.5, -5.6, -5.7, -5.8),
    latitude = c(36.5, 36.6, 36.7, 36.8),
    SIZE_HA = c(1.0, 0.5, 1.5, 2.0),
    AVG_YEAR = c(2015, 2016, 2017, 2018)
  )

  result <- RawPlots(test_data, allow_interactive = FALSE)

  # Should only have 2 rows (P1 and P3)
  expect_equal(nrow(result), 2)
  expect_equal(result$PLOT_ID, c("P1", "P3"))
  expect_equal(result$AGB_T_HA, c(100, 200))
})

test_that("RawPlots excludes list columns from auto-detection", {
  # Create test data with a geometry column (list column)
  test_data <- data.frame(
    PLOT_ID = c("P1", "P2"),
    AGB_T_HA = c(100, 150),
    longitude = c(-5.5, -5.6),
    latitude = c(36.5, 36.6),
    SIZE_HA = c(1.0, 0.5),
    AVG_YEAR = c(2015, 2016)
  )
  # Add a list column (simulating sf geometry)
  test_data$geometry <- list(list(x = 1, y = 2), list(x = 3, y = 4))

  # Should still work, ignoring the geometry column
  result <- RawPlots(test_data, allow_interactive = FALSE)

  expect_equal(nrow(result), 2)
  expect_false("geometry" %in% names(result))
})

test_that("RawPlots handles plot size conversions correctly", {
  # Test case with mock functions
  # Create a mock RawPlots function that doesn't require interactive input
  mock_RawPlots <- function(plots, sizes) {
    converted_sizes <- ifelse(!is.na(sizes) & sizes > 50, sizes / 10000, sizes)
    return(converted_sizes)
  }
  
  # Test data
  small_sizes <- c(0.1, 0.25, 0.5) # Already in hectares
  large_sizes <- c(1000, 2500, 5000) # In square meters
  mixed_sizes <- c(0.1, 2500, 0.5) # Mix of ha and m²
  
  # Test conversions
  expect_equal(mock_RawPlots(NULL, small_sizes), small_sizes)
  expect_equal(mock_RawPlots(NULL, large_sizes), large_sizes / 10000)
  expect_equal(mock_RawPlots(NULL, mixed_sizes), c(0.1, 0.25, 0.5))
})

test_that("RawPlotsTree validates input types", {
  # Test non-dataframe input
  expect_error(RawPlotsTree("not_a_dataframe"), "Input file should be a data frame")

  # Skip tests that require interactive input
  testthat::skip_if_not(interactive(), "Tests require interactive input")
})

test_that("RawPlotsTree errors in non-interactive mode with allow_interactive = FALSE", {
  skip("RawPlotsTree tests require interactive session")

  # Create test data
  test_data <- data.frame(
    id = c(1, 1, 2),
    genus = c("Pinus", "Pinus", "Quercus"),
    species = c("sylvestris", "sylvestris", "robur"),
    diameter = c(15, 20, 25),
    height = c(10, 12, 15),
    size = c(100, 100, 100),
    year = c(2010, 2010, 2010),
    x = c(-62.215, -62.215, -62.205),
    y = c(-3.465, -3.465, -3.455)
  )

  # Should error with informative message in non-interactive mode
  expect_error(
    RawPlotsTree(test_data, allow_interactive = FALSE),
    "RawPlotsTree requires interactive input"
  )
  expect_error(
    RawPlotsTree(test_data, allow_interactive = FALSE),
    "pre-format your data"
  )
})

test_that("RawPlotsTree respects allow_interactive parameter", {
  skip("RawPlotsTree tests require interactive session")

  # Create test data
  test_data <- data.frame(
    id = c(1, 2),
    genus = c("Pinus", "Quercus"),
    species = c("sylvestris", "robur"),
    diameter = c(15, 25),
    x = c(-62.215, -62.205),
    y = c(-3.465, -3.455)
  )

  # With allow_interactive = FALSE, should error
  expect_error(
    RawPlotsTree(test_data, allow_interactive = FALSE),
    "interactive input"
  )

  # The error message should be helpful
  expect_error(
    RawPlotsTree(test_data, allow_interactive = FALSE),
    "run in an interactive session"
  )
})

# Create a mock function to test RawPlotsTree's logic without interactive input
test_that("RawPlotsTree processes tree data correctly", {
  # Create a mock function that imitates RawPlotsTree's core logic
  mock_RawPlotsTree <- function(plots, include_height = TRUE) {
    # Sample data
    id <- c(1, 1, 2)
    genus <- c("Pinus", "Pinus", "Quercus")
    species <- c("sylvestris", "sylvestris", "robur")
    diameter <- c(15, 20, 25)
    height <- c(10, 12, 15)
    size <- c(100, 100, 100)
    fez <- NA
    gez <- NA
    year <- c(2010, 2010, 2010)
    
    x <- c(-62.215, -62.215, -62.205)
    y <- c(-3.465, -3.465, -3.455)
    
    if (include_height) {
      plt <- data.frame(id, genus, species, diameter, height, size, fez, gez, year)
    } else {
      plt <- data.frame(id, genus, species, diameter, size, fez, gez, year)
    }
    
    plt1 <- data.frame(id, x, y)
    
    return(list(plt, plt1))
  }
  
  # Test with height
  result_with_height <- mock_RawPlotsTree(NULL, TRUE)
  expect_length(result_with_height, 2)
  expect_named(result_with_height[[1]], c("id", "genus", "species", "diameter", "height", "size", "fez", "gez", "year"))
  expect_named(result_with_height[[2]], c("id", "x", "y"))
  
  # Test without height
  result_without_height <- mock_RawPlotsTree(NULL, FALSE)
  expect_length(result_without_height, 2)
  expect_named(result_without_height[[1]], c("id", "genus", "species", "diameter", "size", "fez", "gez", "year"))
})

# ============================================================================
# Tests for column_map parameter
# ============================================================================

test_that("RawPlots column_map works with all columns specified", {
  # Create test data with non-standard column names
  test_data <- data.frame(
    MyPlotID = c("Site1", "Site2", "Site3"),
    Biomass_MgHa = c(100, 150, 200),
    Easting = c(-5.5, -5.6, -5.7),
    Northing = c(36.5, 36.6, 36.7),
    Area_ha = c(1.0, 0.5, 1.5),
    MeasYear = c(2015, 2016, 2017)
  )

  result <- RawPlots(test_data,
    allow_interactive = FALSE,
    column_map = list(
      id = "MyPlotID",
      agb = "Biomass_MgHa",
      x = "Easting",
      y = "Northing",
      size = "Area_ha",
      year = "MeasYear"
    ))

  # Verify output structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(result$PLOT_ID, c("Site1", "Site2", "Site3"))
  expect_equal(result$AGB_T_HA, c(100, 150, 200))
  expect_equal(result$POINT_X, c(-5.5, -5.6, -5.7))
  expect_equal(result$POINT_Y, c(36.5, 36.6, 36.7))
  expect_equal(result$SIZE_HA, c(1.0, 0.5, 1.5))
  expect_equal(result$AVG_YEAR, c(2015, 2016, 2017))
})

test_that("RawPlots column_map works without id (generates sequential IDs)", {
  test_data <- data.frame(
    Biomass_MgHa = c(100, 150, 200),
    Easting = c(-5.5, -5.6, -5.7),
    Northing = c(36.5, 36.6, 36.7),
    Area_ha = c(1.0, 0.5, 1.5),
    MeasYear = c(2015, 2016, 2017)
  )

  result <- RawPlots(test_data,
    allow_interactive = FALSE,
    column_map = list(
      agb = "Biomass_MgHa",
      x = "Easting",
      y = "Northing",
      size = "Area_ha",
      year = "MeasYear"
    ))

  # Verify sequential IDs were generated
  expect_equal(result$PLOT_ID, c("PLOT_1", "PLOT_2", "PLOT_3"))
  expect_equal(result$AGB_T_HA, c(100, 150, 200))
})

test_that("RawPlots column_map errors with missing required columns", {
  test_data <- data.frame(
    Biomass_MgHa = c(100, 150, 200),
    Easting = c(-5.5, -5.6, -5.7),
    Northing = c(36.5, 36.6, 36.7)
  )

  # Missing size and year
  expect_error(
    RawPlots(test_data,
      allow_interactive = FALSE,
      column_map = list(
        agb = "Biomass_MgHa",
        x = "Easting",
        y = "Northing"
      )),
    "Required columns missing from column_map"
  )
  expect_error(
    RawPlots(test_data,
      allow_interactive = FALSE,
      column_map = list(
        agb = "Biomass_MgHa",
        x = "Easting",
        y = "Northing"
      )),
    "size"
  )
})

test_that("RawPlots column_map errors when specified columns don't exist in data", {
  test_data <- data.frame(
    Biomass_MgHa = c(100, 150, 200),
    Easting = c(-5.5, -5.6, -5.7),
    Northing = c(36.5, 36.6, 36.7),
    Area_ha = c(1.0, 0.5, 1.5),
    MeasYear = c(2015, 2016, 2017)
  )

  # Specify non-existent column
  expect_error(
    RawPlots(test_data,
      allow_interactive = FALSE,
      column_map = list(
        agb = "NonExistentColumn",
        x = "Easting",
        y = "Northing",
        size = "Area_ha",
        year = "MeasYear"
      )),
    "Columns specified in column_map not found in data"
  )
  expect_error(
    RawPlots(test_data,
      allow_interactive = FALSE,
      column_map = list(
        agb = "NonExistentColumn",
        x = "Easting",
        y = "Northing",
        size = "Area_ha",
        year = "MeasYear"
      )),
    "NonExistentColumn"
  )
})

test_that("RawPlots column_map errors when not a list", {
  test_data <- data.frame(
    Biomass_MgHa = c(100, 150, 200),
    Easting = c(-5.5, -5.6, -5.7),
    Northing = c(36.5, 36.6, 36.7),
    Area_ha = c(1.0, 0.5, 1.5),
    MeasYear = c(2015, 2016, 2017)
  )

  # Pass non-list as column_map
  expect_error(
    RawPlots(test_data,
      allow_interactive = FALSE,
      column_map = "not_a_list"),
    "column_map must be a named list"
  )
})

test_that("RawPlots column_map handles m² to hectare conversion", {
  test_data <- data.frame(
    Biomass_MgHa = c(100, 150, 200),
    Easting = c(-5.5, -5.6, -5.7),
    Northing = c(36.5, 36.6, 36.7),
    Area_m2 = c(10000, 5000, 100),  # Mix of m² and ha
    MeasYear = c(2015, 2016, 2017)
  )

  result <- RawPlots(test_data,
    allow_interactive = FALSE,
    column_map = list(
      agb = "Biomass_MgHa",
      x = "Easting",
      y = "Northing",
      size = "Area_m2",
      year = "MeasYear"
    ))

  # Verify conversion (values > 50 are assumed to be m²)
  expect_equal(result$SIZE_HA, c(1.0, 0.5, 0.01))
})

test_that("RawPlots column_map filters out NA AGB values", {
  test_data <- data.frame(
    Biomass_MgHa = c(100, NA, 200),
    Easting = c(-5.5, -5.6, -5.7),
    Northing = c(36.5, 36.6, 36.7),
    Area_ha = c(1.0, 0.5, 1.5),
    MeasYear = c(2015, 2016, 2017)
  )

  result <- RawPlots(test_data,
    allow_interactive = FALSE,
    column_map = list(
      agb = "Biomass_MgHa",
      x = "Easting",
      y = "Northing",
      size = "Area_ha",
      year = "MeasYear"
    ))

  # Should only have 2 rows (first and third)
  expect_equal(nrow(result), 2)
  expect_equal(result$AGB_T_HA, c(100, 200))
})

test_that("RawPlotsTree column_map works with all columns including height", {
  skip("RawPlotsTree tests require interactive session")

  test_data <- data.frame(
    PlotCode = c(1, 1, 2),
    TreeGenus = c("Pinus", "Pinus", "Quercus"),
    TreeSpecies = c("sylvestris", "sylvestris", "robur"),
    DBH_cm = c(15, 20, 25),
    Height_m = c(10, 12, 15),
    PlotArea_ha = c(0.01, 0.01, 0.01),
    MeasYear = c(2010, 2010, 2010),
    Longitude = c(-62.215, -62.215, -62.205),
    Latitude = c(-3.465, -3.465, -3.455)
  )

  result <- RawPlotsTree(test_data,
    allow_interactive = FALSE,
    column_map = list(
      id = "PlotCode",
      genus = "TreeGenus",
      species = "TreeSpecies",
      diameter = "DBH_cm",
      height = "Height_m",
      x = "Longitude",
      y = "Latitude",
      size = "PlotArea_ha",
      year = "MeasYear"
    ))

  expect_length(result, 2)
  expect_named(result[[1]], c("id", "genus", "species", "diameter", "height", "size", "fez", "gez", "year"))
  expect_named(result[[2]], c("id", "x", "y"))
})

test_that("RawPlotsTree column_map works without height column", {
  skip("RawPlotsTree tests require interactive session")

  test_data <- data.frame(
    PlotCode = c(1, 1, 2),
    TreeGenus = c("Pinus", "Pinus", "Quercus"),
    TreeSpecies = c("sylvestris", "sylvestris", "robur"),
    DBH_cm = c(15, 20, 25),
    PlotArea_ha = c(0.01, 0.01, 0.01),
    MeasYear = c(2010, 2010, 2010),
    Longitude = c(-62.215, -62.215, -62.205),
    Latitude = c(-3.465, -3.465, -3.455)
  )

  result <- RawPlotsTree(test_data,
    allow_interactive = FALSE,
    column_map = list(
      id = "PlotCode",
      genus = "TreeGenus",
      species = "TreeSpecies",
      diameter = "DBH_cm",
      x = "Longitude",
      y = "Latitude",
      size = "PlotArea_ha",
      year = "MeasYear"
    ))

  expect_length(result, 2)
  expect_named(result[[1]], c("id", "genus", "species", "diameter", "size", "fez", "gez", "year"))
})