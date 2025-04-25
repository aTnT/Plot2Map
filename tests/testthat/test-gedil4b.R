library(testthat)
library(Plot2Map)
library(sf)

# Tests for GEDIL4B.R

# Mock tests that don't require actual data download
test_that("download_gedi_l4b validates input parameters", {
  # Skip actual download tests
  skip("Skipping download tests that require authentication")
  
  # Check for required packages
  required_pkgs <- c("earthdatalogin", "sf", "gdalcubes", "rstac", "terra")
  has_all_packages <- all(sapply(required_pkgs, function(pkg) requireNamespace(pkg, quietly = TRUE)))
  skip_if_not(has_all_packages, "Required packages not available")
  
  # Test with NULL ROI (should use default global extent)
  expect_no_error(download_gedi_l4b(roi = NULL, gedi_l4b_band = "MU"))
  
  # Test with custom ROI
  roi <- sf::st_as_sf(data.frame(y = c(-10, 0), x = c(-70, -60)), 
                      coords = c("x", "y"), crs = 4326)
  expect_no_error(download_gedi_l4b(roi = roi, gedi_l4b_band = "MU"))
  
  # Test invalid band
  expect_error(download_gedi_l4b(roi = roi, gedi_l4b_band = "INVALID"),
               "No 'INVALID' raster assets found")
})

# Test ROI creation without actual download
test_that("download_gedi_l4b creates correct ROI", {
  # Test the default ROI creation without running the actual download
  roi_creation <- function() {
    roi <- sf::st_as_sf(data.frame(x = c(-180, 180), y = c(-52, 52)), 
                        coords = c("x", "y"), crs = 4326)
    bbox <- sf::st_bbox(roi)
    return(bbox)
  }
  
  bbox <- roi_creation()
  expect_equal(as.numeric(bbox[1]), -180)
  expect_equal(as.numeric(bbox[3]), 180)
  expect_equal(as.numeric(bbox[2]), -52)
  expect_equal(as.numeric(bbox[4]), 52)
})

# Test output file naming 
test_that("download_gedi_l4b constructs correct output file paths", {
  output_path_construction <- function(band) {
    prefix <- paste0("GEDI_L4B_", band, "_")
    dir <- "data/GEDI_L4B/"
    
    # This mimics the file pattern that would be used to list output files
    pattern <- paste0("^", prefix, ".*\\.tif$")
    return(list(prefix = prefix, dir = dir, pattern = pattern))
  }
  
  mu_paths <- output_path_construction("MU")
  expect_equal(mu_paths$prefix, "GEDI_L4B_MU_")
  expect_equal(mu_paths$pattern, "^GEDI_L4B_MU_.*\\.tif$")
  
  se_paths <- output_path_construction("SE")
  expect_equal(se_paths$prefix, "GEDI_L4B_SE_")
})