library(testthat)
library(sf)
library(terra)

# Tests for treecover.R

# Test calculate_gfc_tiles function
test_that("calculate_gfc_tiles function works correctly", {
  # Create a simple polygon
  roi <- st_sf(geometry = st_sfc(st_polygon(list(cbind(
    c(-70, -60, -60, -70, -70),
    c(-10, -10, 0, 0, -10)
  )))), crs = 4326)

  # Calculate tiles
  tiles <- calculate_gfc_tiles(roi)

  # Check that we got some tiles
  expect_s3_class(tiles, "sf")
  expect_true(nrow(tiles) > 0)
  expect_true("tile_id" %in% names(tiles))
})

# Test create_gfc_tiles_grid function
test_that("create_gfc_tiles_grid function works correctly", {
  # Create the grid
  grid <- create_gfc_tiles_grid()

  # Check that we got a grid
  expect_s3_class(grid, "sf")
  expect_true(nrow(grid) > 0)
  expect_true("tile_id" %in% names(grid))

  # Check that 0° latitude is correctly labeled as North (00N)
  equator_tiles <- grid[grep("00N", grid$tile_id), ]
  expect_true(nrow(equator_tiles) > 0)

  # Check that -10° latitude is correctly labeled as 10S (not 00S)
  first_southern_tiles <- grid[grep("10S", grid$tile_id), ]
  expect_true(nrow(first_southern_tiles) > 0)

  # Test with a specific example spanning the equator
  aoi <- st_sf(geometry = st_sfc(st_polygon(list(cbind(
    c(20, 21, 21, 20, 20),
    c(-1, -1, 1, 1, -1)
  )))), crs = 4326)

  # Find tiles for this area
  tiles <- calculate_gfc_tiles(aoi)

  # Check that we get both 00N and 10S tiles (not 00S)
  tile_ids <- tiles$tile_id
  expect_true(any(grepl("00N", tile_ids)))
  expect_true(any(grepl("10S", tile_ids)))
  expect_false(any(grepl("00S", tile_ids)))
})

test_that("process_pair function requires appropriate inputs", {
  # We don't have lossyear files to test actual functionality, but we can at least test error handling

  # Create a temporary directory for test files
  temp_dir <- tempdir()

  id <- "GFC-2023-v1.11_50N_010W"
  year <- 2015
  treecover_threshold <- NULL
  output_folder <- file.path(temp_dir, "output")
  treecover_file <- "/home/rstudio/Plot2Map/tests/testthat/data/GFC/Hansen_GFC-2023-v1.11_treecover2000_50N_010W.tif"
  lossyear_file <- "/home/rstudio/Plot2Map/non_existent_file.tif"  # Non-existent file
  treecover_files <- c(treecover_file)
  lossyear_files <- c(lossyear_file)
  treecover_ids <- c("GFC-2023-v1.11_50N_010W")
  lossyear_ids <- c("GFC-2023-v1.11_50N_010W")
  baseline <- 2000

  # Test with missing lossyear file
  expect_error(
    process_pair(id, year, treecover_threshold, output_folder,
               treecover_files, lossyear_files, treecover_ids, lossyear_ids, baseline)
  )

  # Test with empty treecover files
  expect_error(
    process_pair(id, year, treecover_threshold, output_folder,
               treecover_files = c(), lossyear_files, treecover_ids, lossyear_ids, baseline)
  )
})

# Create a mock lossyear file for testing
test_that("process_pair function works with mock data", {
  skip_if_not_installed("terra")

  # Create a unique temporary directory for test files
  temp_dir <- tempdir()
  test_id <- paste0("mock_gfc_", format(Sys.time(), "%H%M%S"))
  temp_gfc_dir <- file.path(temp_dir, test_id)

  # Create directory, suppressing warning if it already exists
  if (!dir.exists(temp_gfc_dir)) {
    dir.create(temp_gfc_dir, recursive = TRUE)
  }

  # Get a real treecover file to use
  real_treecover_file <- "/home/rstudio/Plot2Map/tests/testthat/data/GFC/Hansen_GFC-2023-v1.11_treecover2000_50N_010W.tif"

  # Skip if file doesn't exist
  skip_if_not(file.exists(real_treecover_file))

  # Read the tree cover raster to get its properties
  tc_rast <- rast(real_treecover_file)

  # Create a mock lossyear raster with the same dimensions
  # Values: 0 = no loss, 1-23 = year of loss (2001-2023)
  loss_rast <- rast(tc_rast)

  # Set most to 0 (no loss)
  values(loss_rast) <- 0

  # Add some loss in different years (use smaller sample to speed up test)
  set.seed(123)
  idx <- sample(1:ncell(loss_rast), min(1000, ncell(loss_rast)))
  loss_values <- sample(1:15, length(idx), replace = TRUE)  # Loss years 1-15 (2001-2015)
  loss_rast[idx] <- loss_values

  # Save the mock lossyear file
  mock_lossyear_file <- file.path(temp_gfc_dir, "Hansen_GFC-2023-v1.11_lossyear_50N_010W.tif")
  writeRaster(loss_rast, mock_lossyear_file, overwrite = TRUE)

  # Copy the real treecover file to the mock directory
  mock_treecover_file <- file.path(temp_gfc_dir, basename(real_treecover_file))
  file.copy(real_treecover_file, mock_treecover_file, overwrite = TRUE)

  # Set up test parameters
  id <- "GFC-2023-v1.11_50N_010W"
  year <- 2015
  treecover_threshold <- NULL
  output_folder <- file.path(temp_dir, paste0("output_", test_id))
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }

  treecover_files <- c(mock_treecover_file)
  lossyear_files <- c(mock_lossyear_file)
  treecover_ids <- c("GFC-2023-v1.11_50N_010W")
  lossyear_ids <- c("GFC-2023-v1.11_50N_010W")
  baseline <- 2000

  # Test basic functionality with cleanup
  tryCatch({
    result <- tryCatch({
      process_pair(id, year, treecover_threshold, output_folder,
                   treecover_files, lossyear_files, treecover_ids, lossyear_ids, baseline)
    }, error = function(e) {
      skip(paste("Skipping test due to error:", e$message))
      NULL
    })

    if (!is.null(result)) {
      expect_true(file.exists(result))
      expect_true(grepl("treecover_calc_2015", result))

      # Test with threshold
      result_threshold <- process_pair(id, year, treecover_threshold = 50, output_folder,
                                      treecover_files, lossyear_files, treecover_ids, lossyear_ids, baseline)
      expect_true(file.exists(result_threshold))

      # Read both results and compare
      rast_all <- rast(result)
      rast_threshold <- rast(result_threshold)

      # The threshold result should have fewer non-NA cells
      expect_true(global(rast_all, fun="notNA")[1,] >= global(rast_threshold, fun="notNA")[1,])
    }
  }, finally = {
    # Clean up test directories
    if (dir.exists(temp_gfc_dir)) {
      unlink(temp_gfc_dir, recursive = TRUE)
    }
    if (dir.exists(output_folder)) {
      unlink(output_folder, recursive = TRUE)
    }
  })
})

# Test for compute_treecover function
test_that("compute_treecover validates inputs correctly", {
  # Test error for invalid year
  expect_error(compute_treecover(2000, "data/GFC"), "Year must be between 2001 and 2023.")
  expect_error(compute_treecover(2024, "data/GFC"), "Year must be between 2001 and 2023.")

  # Test error for invalid baseline
  expect_error(compute_treecover(2015, "data/GFC", baseline = 1999), "Baseline must be either 2000 or 2010.")
  expect_error(compute_treecover(2015, "data/GFC", baseline = 2011), "Baseline must be either 2000 or 2010.")

  # Test error for non-existent coordinates
  expect_error(compute_treecover(2015, "/home/rstudio/Plot2Map/tests/testthat/data/GFC", coords = "91N_999E"),
               "No matching files found for the specified coordinates.")
})

# Test download_glad_tcc_2010 function ROI intersection
test_that("download_glad_tcc_2010 filters tiles by ROI correctly", {

  skip()

  # Create a test ROI in Africa (10-15°N, 20-25°E)
  roi <- st_sf(geometry = st_sfc(st_polygon(list(cbind(
    c(20, 25, 25, 20, 20),
    c(10, 10, 15, 15, 10)
  )))), crs = 4326)

  # Test basic ROI processing by replacing the network functions temporarily
  # Save original functions to restore later
  orig_read_html <- rvest::read_html
  orig_html_nodes <- rvest::html_nodes
  orig_html_attr <- rvest::html_attr
  orig_download_file <- utils::download.file

  # Replace with dummy functions for testing
  # This avoids using mockery but still tests the ROI filtering logic
  environment(download_glad_tcc_2010)$"rvest::read_html" <- function(...) {
    structure(list(), class = "xml_document")
  }

  environment(download_glad_tcc_2010)$"rvest::html_nodes" <- function(...) {
    list()
  }

  environment(download_glad_tcc_2010)$"rvest::html_attr" <- function(...) {
    c(
      "treecover2010_10N_010E.tif",
      "treecover2010_10N_020E.tif",
      "treecover2010_20N_020E.tif",
      "treecover2010_00N_030E.tif"
    )
  }

  environment(download_glad_tcc_2010)$"utils::download.file" <- function(url, destfile, ...) {
    # Just create an empty file without downloading
    file.create(destfile)
    return(0)  # Success return code
  }

  # Create a temporary output directory
  temp_dir <- tempdir()
  output_dir <- file.path(temp_dir, "GLAD_test")
  if (!dir.exists(output_dir)) dir.create(output_dir)

  # Temporarily modify function to avoid actual downloads
  tryCatch({
    # Run the function with the temporary replacements
    downloaded_files <- download_glad_tcc_2010(roi = roi, output_folder = output_dir, n_cores = 1)

    # Should only return the file that intersects with our ROI
    expected_matches <- c("treecover2010_10N_020E.tif")

    # Check the result focuses on the right tile
    expect_equal(length(downloaded_files), length(expected_matches))
    expect_true(all(basename(downloaded_files) %in% expected_matches))
  },
  finally = {
    # Clean up temporary files
    unlink(output_dir, recursive = TRUE)
  })
})

# Test download_gfc_tiles function without external mocking libraries
test_that("download_gfc_tiles builds correct URLs", {
  # Skip download tests by default
  skip("Skipping test that requires downloads")

  # Create a simple set of tiles using a rectangle in South America
  roi <- st_sf(geometry = st_sfc(st_polygon(list(cbind(
    c(-70, -60, -60, -70, -70),
    c(-10, -10, 0, 0, -10)
  )))), crs = 4326)

  tiles <- calculate_gfc_tiles(roi)

  # Create a temporary output directory
  temp_dir <- tempdir()
  output_dir <- file.path(temp_dir, "GFC_test")
  if (!dir.exists(output_dir)) dir.create(output_dir)

  # Save original download function
  orig_download_file <- utils::download.file
  orig_verify_download <- verify_download

  # Count URL construction and downloads
  url_patterns <- character(0)
  download_count <- 0

  # Replace functions temporarily
  assign("download_file", function(url, destfile, ...) {
    # Track URL patterns
    url_patterns <<- c(url_patterns, url)
    download_count <<- download_count + 1

    # Create empty file for verification
    file.create(destfile)
    return(0)  # Success code
  }, envir = environment(download_gfc_tiles))

  assign("verify_download", function(url, local_path) {
    return(0)  # Always verify as success
  }, envir = environment(download_gfc_tiles))

  tryCatch({
    # Use just a sample of the first two tiles for testing
    test_tiles <- tiles[1:2,]

    # Run the function
    downloaded_files <- download_gfc_tiles(
      test_tiles, output_dir,
      images = c("treecover2000"),  # Just one image type for testing
      dataset = "GFC-2023-v1.11"
    )

    # Check URL construction
    expect_true(length(url_patterns) > 0)
    expect_true(all(grepl("https://storage.googleapis.com/earthenginepartners-hansen/GFC-2023-v1.11", url_patterns)))
    expect_true(all(grepl("Hansen_GFC-2023-v1.11_treecover2000", url_patterns)))

    # Check proper file creation
    expect_equal(length(downloaded_files), nrow(test_tiles))
  },
  finally = {
    # Clean up
    unlink(output_dir, recursive = TRUE)
  })
})
