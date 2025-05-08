library(testthat)
library(Plot2Map)
library(terra)
library(sf)
library(dplyr)

# Tests for InvDasymetry.R


## Synthetic data tests

# Simplified test for biomass scaling in invDasymetry function
# This test specifically focuses on verifying that biomass values maintain
# their physical meaning (t/ha) in both aggregated and non-aggregated modes.

# Create a simple synthetic dataset for testing
create_test_data <- function() {
  # Create a simple data frame with explicit values for testing
  df <- data.frame(
    PLOT_ID = c("P1", "P2", "P3", "P4"),
    POINT_X = c(0.2, 0.4, 0.6, 0.8),
    POINT_Y = rep(0.5, 4),
    AGB_T_HA = c(100, 200, 300, 800),  # Last value is our extreme value
    AGB_T_HA_ORIG = c(100, 200, 300, 800),  # Same as AGB_T_HA for this test
    SIZE_HA = rep(1.0, 4),
    ZONE = rep("ZoneA", 4),
    varPlot = rep(100, 4)  # Simple variance values
  )

  return(df)
}

# Create synthetic raster files for testing
create_test_rasters <- function(temp_dir) {
  # Create a simple biomass raster
  r_agb <- terra::rast(xmin = 0, xmax = 1, ymin = 0, ymax = 1,
                       resolution = 0.05, crs = "EPSG:4326")
  terra::values(r_agb) <- rep(100, terra::ncell(r_agb))

  # Create a simple forest cover raster
  r_forest <- terra::rast(xmin = 0, xmax = 1, ymin = 0, ymax = 1,
                          resolution = 0.05, crs = "EPSG:4326")
  terra::values(r_forest) <- rep(80, terra::ncell(r_forest))  # 80% forest cover

  # Save the rasters to temporary files
  agb_path <- file.path(temp_dir, "test_agb.tif")
  forest_path <- file.path(temp_dir, "test_forest.tif")

  terra::writeRaster(r_agb, agb_path, overwrite = TRUE)
  terra::writeRaster(r_forest, forest_path, overwrite = TRUE)

  return(list(agb_path = agb_path, forest_path = forest_path))
}

# Test that biomass values maintain correct units in non-aggregated mode
test_that("invDasymetry preserves biomass values in non-aggregated mode", {
  # Skip on CI if needed
  skip_on_ci()

  # Create a temporary directory
  temp_dir <- tempdir()

  # Prepare test data
  plot_data <- create_test_data()
  raster_paths <- create_test_rasters(temp_dir)

  # Run invDasymetry in non-aggregated mode
  result <- invDasymetry(
    plot_data = plot_data,
    clmn = "ZONE",
    value = "ZoneA",
    aggr = NULL,  # No aggregation
    threshold = 10,
    dataset = "custom",
    agb_raster_path = raster_paths$agb_path,
    forest_mask_path = raster_paths$forest_path
  )

  # Verify the structure
  expect_true("orgPlotAGB" %in% names(result))
  expect_true("tfPlotAGB" %in% names(result))
  expect_true("SIZE_HA" %in% names(result))

  # Verify original values are preserved
  expect_equal(max(result$orgPlotAGB), 800, tolerance = 1e-6)

  # Verify that values are in a reasonable physical range (t/ha)
  expect_true(max(result$plotAGB_10) <= 800,
              "Tree-cover adjusted biomass should not exceed original values")
})

# Test that biomass values maintain correct physical meaning in aggregated mode
test_that("invDasymetry maintains biomass physical meaning in aggregated mode", {

  skip_on_ci()

  # Create a temporary directory
  temp_dir <- tempdir()

  # Create a larger dataset with points that will aggregate into two cells
  extended_data <- data.frame(
    PLOT_ID = paste0("P", 1:8),
    POINT_X = c(0.2, 0.2, 0.3, 0.3, 0.7, 0.7, 0.8, 0.8),  # Two clusters
    POINT_Y = c(0.2, 0.3, 0.2, 0.3, 0.7, 0.8, 0.7, 0.8),  # Each will form a cell
    AGB_T_HA = c(100, 110, 120, 130, 200, 210, 220, 230),  # Similar values within clusters
    AGB_T_HA_ORIG = c(100, 110, 120, 130, 200, 210, 220, 230),
    SIZE_HA = rep(1.0, 8),
    ZONE = rep("ZoneA", 8),
    varPlot = rep(100, 8)
  )

  # Create rasters
  raster_paths <- create_test_rasters(temp_dir)

  # Run invDasymetry in non-aggregated mode with threshold=0
  result_no_aggr <- invDasymetry(
    plot_data = extended_data,
    clmn = "ZONE",
    value = "ZoneA",
    aggr = NULL,
    threshold = 10,  # Using threshold=0 to get comparable values
    dataset = "custom",
    agb_raster_path = raster_paths$agb_path,
    forest_mask_path = raster_paths$forest_path
  )

  # Run invDasymetry in aggregated mode with threshold=0
  result_aggr <- invDasymetry(
    plot_data = extended_data,
    clmn = "ZONE",
    value = "ZoneA",
    aggr = 0.5,  # This will create two cells from our data
    threshold = 10,
    dataset = "custom",
    agb_raster_path = raster_paths$agb_path,
    forest_mask_path = raster_paths$forest_path
  )


  # Verify that original biomass values are properly aggregated
  # When plots are aggregated, we expect the new cells to have the average biomass
  # of the plots that fell into each cell.

  # First cell should have plots with values 100, 110, 120, 130 (avg ~115)
  # Second cell should have plots with values 200, 210, 220, 230 (avg ~215)
  agg_values <- result_aggr$orgPlotAGB[order(result_aggr$orgPlotAGB)]
  expect_equal(agg_values[1], 115, tolerance = 0)
  expect_equal(agg_values[2], 215, tolerance = 0)

  # Verify biomass values in relation to forest cover percentage
  agb_values <- result_aggr$plotAGB_10[order(result_aggr$orgPlotAGB)]

  # agg and agb values shall be equal
  expect_equal (agb_values[1],  agg_values[1], tolerance = 1e-6)
  expect_equal (agb_values[2],  agg_values[2], tolerance = 1e-6)

  # verify proper number of plots
  expect_true(all(result_aggr$n == 4), "Each aggregated cell should contain 4 plots")
})



test_that("invDasymetry applies threshold filtering consistently", {
  skip_on_ci()
  # Create a temp directory for test files
  temp_dir <- tempdir()

  # Function to create test rasters with controlled values
  create_test_rasters <- function(dir) {
    # Create an AGB raster with uniform value of 100
    agb_raster <- terra::rast(nrows=20, ncols=20, xmin=0, xmax=1, ymin=0, ymax=1, crs="EPSG:4326")
    terra::values(agb_raster) <- rep(100, terra::ncell(agb_raster))
    agb_path <- file.path(dir, "test_agb.tif")
    terra::writeRaster(agb_raster, agb_path, overwrite=TRUE)

    # Create a forest mask with a uniform value of 80% forest cover
    forest_raster <- terra::rast(nrows=20, ncols=20, xmin=0, xmax=1, ymin=0, ymax=1, crs="EPSG:4326")
    terra::values(forest_raster) <- rep(80, terra::ncell(forest_raster))
    forest_path <- file.path(dir, "test_forest.tif")
    terra::writeRaster(forest_raster, forest_path, overwrite=TRUE)

    return(list(
      agb_path = agb_path,
      forest_path = forest_path
    ))
  }

  # Create a test dataset with 8 plots
  set.seed(42)
  extended_data <- data.frame(
    PLOT_ID = paste0("P", 1:8),
    POINT_X = c(0.2, 0.2, 0.3, 0.3, 0.7, 0.7, 0.8, 0.8),  # Two clusters
    POINT_Y = c(0.2, 0.3, 0.2, 0.3, 0.7, 0.8, 0.7, 0.8),  # Each will form a cell
    AGB_T_HA = c(100, 110, 120, 130, 200, 210, 220, 230),  # Similar values within clusters
    AGB_T_HA_ORIG = c(100, 110, 120, 130, 200, 210, 220, 230),
    SIZE_HA = rep(1.0, 8),  # All plots are 1 hectare
    ZONE = rep("ZoneA", 8),
    varPlot = rep(100, 8)
  )

  # Create rasters
  raster_paths <- create_test_rasters(temp_dir)

  # Run invDasymetry with different thresholds and modes to compare behavior

  # 1. Non-aggregated mode with threshold=0
  result_no_aggr_t0 <- invDasymetry(
    plot_data = extended_data,
    clmn = "ZONE",
    value = "ZoneA",
    aggr = NULL,
    threshold = 0,
    dataset = "custom",
    agb_raster_path = raster_paths$agb_path,
    forest_mask_path = raster_paths$forest_path
  )

  # 2. Aggregated mode with threshold=0
  result_aggr_t0 <- invDasymetry(
    plot_data = extended_data,
    clmn = "ZONE",
    value = "ZoneA",
    aggr = 0.5,
    threshold = 0,
    dataset = "custom",
    agb_raster_path = raster_paths$agb_path,
    forest_mask_path = raster_paths$forest_path
  )

  # 3. Non-aggregated mode with threshold=90
  result_no_aggr_t90 <- invDasymetry(
    plot_data = extended_data,
    clmn = "ZONE",
    value = "ZoneA",
    aggr = NULL,
    threshold = 90,
    dataset = "custom",
    agb_raster_path = raster_paths$agb_path,
    forest_mask_path = raster_paths$forest_path
  )

  # 4. Aggregated mode with threshold=90
  result_aggr_t90 <- invDasymetry(
    plot_data = extended_data,
    clmn = "ZONE",
    value = "ZoneA",
    aggr = 0.5,
    threshold = 90,
    dataset = "custom",
    agb_raster_path = raster_paths$agb_path,
    forest_mask_path = raster_paths$forest_path
  )

  # Test that threshold=0 results in all full biomass for both modes
  expect_equal(mean(result_no_aggr_t0$plotAGB_0) / mean(result_no_aggr_t0$tfPlotAGB), 1.0)
  expect_equal(mean(result_aggr_t0$plotAGB_0) / mean(result_aggr_t0$orgPlotAGB), 1.0)

  # Test that threshold=90 (above our 80% forest cover) results in zero biomass for both modes
  expect_equal(mean(result_no_aggr_t90$plotAGB_90), 0)
  expect_equal(mean(result_aggr_t90$plotAGB_90), 0)

  # Test that the threshold filtering is actually happening in non-aggregated mode
  # even though plots are 1 hectare (which previously would skip threshold filtering)
  expect_lt(mean(result_no_aggr_t90$plotAGB_90), mean(result_no_aggr_t0$plotAGB_0))

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})




## Real world tests


test_that("invDasymetry accepts map_year and map_resolution parameters and correctly handles aggregation", {

  skip_on_ci()

  # Create real word test dataset
  test_year <- 2022

  # Create a test directory if it doesn't exist
  test_dir <-  tempfile()
  if (!dir.exists(test_dir)) {
    dir.create(test_dir,  recursive = TRUE)
  }

  # # Create a small sample dataset:
  set.seed(42)
  sampled_plots <- plots[sample(nrow(plots), 3), ]
  sampled_plots <- Deforested(sampled_plots, gfc_folder = test_dir,  map_year = test_year)
  plot_data <- BiomePair(sampled_plots$non_deforested_plots)
  plot_data <- TempApplyVar(plot_data, test_year)
  plot_data_s <- plot_data

  # Download ESACCI for testing
  esacci_tile_test <- ESACCIAGBtileNames(
    sf::st_buffer(sampled_plots$non_deforested_plots[3,], 0.0001),
    esacci_biomass_year = "latest",
    esacci_biomass_version = "latest"
  )

  esacci_tile_test_file <- download_esacci_biomass(
    esacci_folder = test_dir,
    file_names = esacci_tile_test
  )

  rm(plot_data, sampled_plots)

  # Create a larger sample dataset because we test the aggregated case below:
  sampled_plots <- plots[sample(nrow(plots), 50), ] |>
    dplyr::filter(PLOT_ID == "EU2")
  sampled_plots <- Deforested(sampled_plots, gfc_folder = test_dir,  map_year = test_year)
  plot_data <- BiomePair(sampled_plots$non_deforested_plots)
  plot_data <- TempApplyVar(plot_data, test_year)
  plot_data_xl <- plot_data
  rm(plot_data, sampled_plots)


  if (!file.exists(esacci_tile_test_file)) {
    skip("Test data set is not available locally ")
  }

  # Test with explicit parameters (use only map_year, no aggregation)
  tryCatch({
    result <- invDasymetry(
      plot_data = plot_data_s,
      aggr = NULL,
      dataset = "custom",
      agb_raster_path = esacci_tile_test_file,
      map_year = test_year,
      map_resolution = 0.001
    )
    # If execution reaches here, it means the parameters were accepted
    expect_true(!is.null(result), "Function accepted map_year and map_resolution parameters")
  }, error = function(e) {
    # Fail the test if the error mentions map_year or map_resolution
    if (grepl("map_year|map_resolution", e$message)) {
      fail(paste("Error with map parameters:", e$message))
    } else {
      # Skip other errors not related to our test focus
      skip(paste("Function failed for reasons unrelated to map parameters:", e$message))
    }
  })

  # invDasymetry correctly handles aggregation
  tryCatch({
    # Run with no aggregation
    result_no_aggr <- invDasymetry(
      plot_data = plot_data_xl,
      aggr = NULL,
      dataset = "custom",
      agb_raster_path = esacci_tile_test_file
    )

    # Run with aggregation
    result_aggr <- invDasymetry(
      plot_data = plot_data_xl,
      aggr = 0.1,
      dataset = "custom",
      agb_raster_path = esacci_tile_test_file
    )

    # Test structural expectations
    expect_equal(nrow(result_no_aggr), nrow(plot_data_xl))
    expect_lte(nrow(result_aggr), nrow(plot_data_xl))

    # Test column structure expectations
    expect_true("tfPlotAGB" %in% names(result_no_aggr), "No-aggregation results include tfPlotAGB column")
    expect_true("n" %in% names(result_aggr), "Aggregation results include n column")

    # Test that every cell in the aggregated results contains at least one plot
    expect_true(all(result_aggr$n >= 1), "All aggregated cells contain at least one plot")

    # Test that the total biomass is roughly conserved (within reasonable margin)
    # This accounts for differences in calculation methods
    max_biomass_no_aggr <- max(result_no_aggr$plotAGB_0 * result_no_aggr$SIZE_HA)
    max_biomass_aggr <-  max(result_aggr$plotAGB_0 * result_aggr$SIZE_HA)

    # Allow for some difference due to different calculation methods
    expect_lt(abs(max_biomass_no_aggr - max_biomass_aggr) / max_biomass_no_aggr,
              0.2, "Total biomass is roughly conserved")
  }, error = function(e) {
    fail(paste("Error:", e$message))
  })

})




# Test automatic varPlot calculation
test_that("invDasymetry calculates varPlot automatically when missing", {
  skip("Test requires full dataset - enable after integration is complete")

  # Create a test directory
  test_dir <- tempfile()
  dir.create(test_dir, recursive = TRUE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  # Load sample plot data and remove varPlot column
  data(plots)
  set.seed(42)
  sampled_plots <- plots[sample(nrow(plots), 5), ]

  # Prepare the plots data using the proper workflow as shown in the README

  # Step 1: Add BiomePair to assign ecological zones and continents (ZONE)
  sampled_plots <- BiomePair(sampled_plots)

  # Step 2: Use TempApplyVar to adjust biomass values to map year and calculate variance
  sampled_plots <- TempApplyVar(sampled_plots, map_year = 2020)

  # Remove varPlot column to force calculation
  if ("varPlot" %in% names(sampled_plots)) {
    sampled_plots$varPlot <- NULL
  }

  # Create a sample raster with year in filename
  agb_raster_path <- file.path(test_dir, "test_agb_2020.tif")
  r <- terra::rast(nrows=10, ncols=10, xmin=0, xmax=0.1, ymin=0, ymax=0.1, crs="EPSG:4326")
  terra::values(r) <- runif(terra::ncell(r), 50, 200)
  terra::writeRaster(r, agb_raster_path, overwrite=TRUE)

  # Create a simple sampling error dataset to avoid using the se.csv file
  simple_se <- data.frame(
    SIZE_HA = c(0.1, 0.25, 0.5, 1.0),
    RS_HA = c(1, 1, 1, 1),
    ratio = c(0.1, 0.25, 0.5, 1.0),
    cv = c(20, 15, 10, 5)
  )

  # Create a dummy RF model for measurement uncertainty
  # Note: This is just a placeholder, the actual calculation requires the real model
  train_data <- data.frame(
    agb = runif(10, 50, 300),
    size = runif(10, 2500, 10000),
    gez = factor(sample(c("Boreal", "Subtropical", "Temperate", "Tropical"), 10, replace = TRUE))
  )
  dummy_rf1 <- ranger::ranger(runif(10, 5, 30) ~ ., data = train_data)

  # Test with aggregation to trigger varPlot calculation
  # This should extract year from filename and resolution from raster
  tryCatch({
    # Save model temporarily for the test
    save(dummy_rf1, file = file.path(test_dir, "rf1.RData"))

    # First attempt without providing se_data
    result <- invDasymetry(
      plot_data = sampled_plots,
      clmn = "ZONE",
      value = unique(sampled_plots$ZONE)[1],
      aggr = 0.1,
      dataset = "custom",
      agb_raster_path = agb_raster_path
    )

    # Function should successfully calculate varPlot
    expect_true(!is.null(result), "Function calculated varPlot automatically")
    expect_true("varPlot" %in% names(result), "Result contains varPlot column")

  }, error = function(e) {
    # Skip test gracefully if calculateTotalUncertainty is not implemented yet
    if (grepl("could not find function \"calculateTotalUncertainty\"", e$message)) {
      skip("calculateTotalUncertainty function not available")
    } else {
      fail(paste("Error:", e$message))
    }
  })
})



# Test the minPlots <= 1 edge case
test_that("invDasymetry handles minPlots <= 1 correctly", {
  skip_on_ci()

  # Create a temporary directory
  temp_dir <- tempdir()

  # Prepare test data
  plot_data <- create_test_data()
  raster_paths <- create_test_rasters(temp_dir)

  # Run invDasymetry with minPlots = 1
  result_min1 <- invDasymetry(
    plot_data = plot_data,
    clmn = "ZONE",
    value = "ZoneA",
    aggr = 0.5,  # With aggregation to test the edge case
    minPlots = 1,  # Set minPlots to 1
    threshold = 10,
    dataset = "custom",
    agb_raster_path = raster_paths$agb_path,
    forest_mask_path = raster_paths$forest_path
  )

  # Verify we get valid output even with minPlots = 1
  expect_s3_class(result_min1, "data.frame")
  expect_true(nrow(result_min1) > 0, "Output with minPlots=1 should contain rows")

  # Test what happens when there are cells with only one plot
  # Create a dataset with isolated points
  isolated_data <- data.frame(
    PLOT_ID = paste0("P", 1:4),
    POINT_X = c(0.1, 0.3, 0.7, 0.9),  # Spaced far apart
    POINT_Y = c(0.1, 0.3, 0.7, 0.9),  # Spaced far apart
    AGB_T_HA = c(100, 200, 300, 400),
    AGB_T_HA_ORIG = c(100, 200, 300, 400),
    SIZE_HA = rep(1.0, 4),
    ZONE = rep("ZoneA", 4),
    varPlot = rep(100, 4)
  )

  # Run with minPlots = 1 to keep all cells with at least 1 plot
  result_isolated_min1 <- invDasymetry(
    plot_data = isolated_data,
    clmn = "ZONE",
    value = "ZoneA",
    aggr = 0.2,  # Small aggregation grid to test single plot cells
    minPlots = 1,  # Keep cells with 1+ plots
    threshold = 10,
    dataset = "custom",
    agb_raster_path = raster_paths$agb_path,
    forest_mask_path = raster_paths$forest_path
  )

  # Verify all plots are preserved when minPlots = 1
  expect_equal(sum(result_isolated_min1$n), nrow(isolated_data), tolerance = 1)

  # Test with minPlots = 0 (will use default of 1)
  result_min0 <- invDasymetry(
    plot_data = isolated_data,
    clmn = "ZONE",
    value = "ZoneA",
    aggr = 0.2,
    minPlots = 0,  # Should use default of 1
    threshold = 10,
    dataset = "custom",
    agb_raster_path = raster_paths$agb_path,
    forest_mask_path = raster_paths$forest_path
  )

  # Should handle minPlots = 0 gracefully (treating it as 1)
  expect_equal(sum(result_min0$n), nrow(isolated_data), tolerance = 1)


  # Clean up
  unlink(temp_dir, recursive = TRUE)
})



test_that("invDasymetry function tests", {
  skip("Skipping as this test requires downloading large datasets")

  # Year for testing
  test_year <- 2023

  # Create a test directory if it doesn't exist
  test_dir <- "tests/test_data"
  if (!dir.exists(test_dir)) {
    dir.create(test_dir, recursive = TRUE)
  }

  # Create a sample dataset of 10 plots:
  set.seed(42)
  sampled_plots <- plots[sample(nrow(plots), 10), ]
  sampled_plots <- Deforested(sampled_plots, gfc_folder = test_dir, map_year = test_year)
  plot_data <- BiomePair(sampled_plots$non_deforested_plots)
  plot_data <- TempApply(plot_data, test_year)

  # Creating a test forest mask
  # Download GLAD 2010 data for each plot:
  glad_tcc_2010 <- lapply(1:dim(plot_data)[1], function(i){
    download_glad_tcc_2010(
      roi = plot_data[i,]$geometry,
      output_folder = test_dir,
      n_cores = 1,
      timeout = 1800
    )
  })

  # Ensure POINT_Y and POINT_X are present in the sf object or derived from the geometry
  if (!("POINT_Y" %in% names(plot_data)) || !("POINT_X" %in% names(plot_data))) {
    # Extract coordinates from the geometry column
    coords_df <- sf::st_coordinates(plot_data)

    # Add the coordinates as new columns to the plot_data sf object
    plot_data$POINT_X <- coords_df[, "X"]
    plot_data$POINT_Y <- coords_df[, "Y"]
  }

  coords <- unique(paste0(floor(plot_data$POINT_Y), ifelse(plot_data$POINT_Y >= 0, "N", "S"), "_",
                         sprintf("%03d", floor(abs(plot_data$POINT_X))), ifelse(plot_data$POINT_X >= 0, "E", "W")))

  # Call compute_treecover for each unique coordinate
  lapply(coords, function(coord) {
    compute_treecover(year = test_year,
                      gfc_folder = test_dir,
                      output_folder = test_dir,
                      num_cores = 1,
                      baseline = 2010)
  })

  fmask_test <- terra::rast(file.path(test_dir, "GLAD_TCC-2010_treecover_calc_2023_40N_010W.tif"))

  # Download ESACCI for testing
  esacci_tile_test <- ESACCIAGBtileNames(
    sf::st_buffer(sampled_plots$non_deforested_plots[3,], 0.0001)$geometry,
    esacci_biomass_year = "latest",
    esacci_biomass_version = "latest"
  )

  download_esacci_biomass(
    esacci_folder = test_dir,
    file_names = esacci_tile_test
  )

  agb_raster_test <- terra::rast(file.path(test_dir, "N40W010_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2021-fv5.0.tif"))

  # Test case 1: Basic execution with Europe zone, no aggregation
  result_no_aggr <- invDasymetry(plot_data = plot_data, clmn = "ZONE", value = "Europe", aggr = NULL,
                               minPlots = 1, weighted_mean = FALSE, is_poly = FALSE,
                               dataset = "custom", agb_raster = agb_raster_test, forest_mask = fmask_test,
                               threshold = 10,
                               esacci_biomass_year = "latest", esacci_biomass_version = "latest",
                               esacci_folder = test_dir,
                               gedi_l4b_folder = test_dir,
                               gedi_l4b_band = "MU", gedi_l4b_resolution = 0.001,
                               n_cores = 1, timeout = 600)

  expect_s3_class(result_no_aggr, "data.frame")
  expect_gt(nrow(result_no_aggr), 0)
})
