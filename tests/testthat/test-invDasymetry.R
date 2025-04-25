library(testthat)
library(Plot2Map)
library(terra)
library(sf)

# Tests for InvDasymetry.R

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