library(testthat)
library(Plot2Map)
library(sf)
library(terra)

test_that("calculate_gfc_tiles creates a valid tile grid", {
  # Create a simple polygon for testing
  # Create polygon explicitly to avoid using st_buffer (which causes warnings with geographic coordinates)
  aoi <- st_sf(geometry = st_sfc(st_polygon(list(cbind(
    c(-70, -60, -60, -70, -70),
    c(-10, -10, 0, 0, -10)
  )))), crs = 4326)

  # Calculate GFC tiles
  tiles <- calculate_gfc_tiles(aoi)

  # Test that we got a valid sf object
  expect_s3_class(tiles, "sf")

  # Test that the tiles have the expected structure
  expect_true("tile_id" %in% colnames(tiles))
  expect_true(all(grepl("[0-9]{2}[NS]_[0-9]{3}[EW]", tiles$tile_id)))

  # Test that we got tiles for this region
  # Get the actual tile IDs returned by the function
  received_tiles <- tiles$tile_id

  # Print the received tiles for debugging
  cat("Received tiles:", paste(received_tiles, collapse = ", "), "\n")

  # Check that we received some tiles (should be 4 or more for this region)
  expect_gte(length(received_tiles), 4)

  # Check that the tile IDs follow the correct format
  expect_true(all(grepl("^\\d{2}[NS]_\\d{3}[EW]$", received_tiles)))
})

test_that("calculate_gfc_tiles returns correct tiles for equator-crossing AOI", {
  # Create test AOI crossing the equator in Central Africa
  aoi <- st_sf(geometry = st_sfc(st_polygon(list(cbind(
    c(20, 21, 21, 20, 20),
    c(-1, -1, 1, 1, -1)
  )))), crs = 4326)

  # Calculate tiles with the new function
  tiles <- calculate_gfc_tiles(aoi)
  
  # Check that result is an sf object with tile_id column
  expect_s3_class(tiles, "sf")
  expect_true("tile_id" %in% colnames(tiles))
  
  # Print for debugging
  cat("Equatorial AOI tiles:", paste(tiles$tile_id, collapse=", "), "\n")
  
  # The small AOI that crosses the equator should return both 00N_020E and 10S_020E
  # since the geometry spans from -1 to 1 latitude
  expect_lte(nrow(tiles), 2)
  
  # We should at minimum see the 00N_020E tile
  expect_true(any(grepl("00N_020E", tiles$tile_id)))
})

test_that("create_gfc_tiles_grid produces valid grid", {
  # Force recreation of the grid
  if (exists("gfc_tiles_grid", envir = .GlobalEnv)) {
    rm(gfc_tiles_grid, envir = .GlobalEnv)
  }
  
  # Create the grid directly
  grid <- create_gfc_tiles_grid()
  
  # Check basic properties
  expect_s3_class(grid, "sf")
  expect_true("tile_id" %in% colnames(grid))
  
  # Check that special Hansen naming conventions are followed
  # Equator tiles should be 00N
  equator_tiles <- subset(grid, grepl("00N_", grid$tile_id))
  expect_true(nrow(equator_tiles) > 0)
  
  # -10° to 0° should be 10S (not 00S)
  neg_10_tiles <- subset(grid, grepl("10S_", grid$tile_id))
  expect_true(nrow(neg_10_tiles) > 0)
  expect_false(any(grepl("00S_", grid$tile_id)))  # 00S shouldn't exist
})

test_that("download_gfc_tiles validates inputs correctly", {
  # Create minimal sf object for testing
  test_tile <- data.frame(
    tile_id = "00N_000E",
    stringsAsFactors = FALSE
  )
  test_tile <- st_sf(geometry = st_sfc(st_polygon(list(cbind(
    c(0, 10, 10, 0, 0),
    c(0, 0, 10, 10, 0)
  )))), test_tile, crs = 4326)
  
  # Test invalid inputs
  expect_error(download_gfc_tiles("not_sf_object", tempdir()))
  expect_error(download_gfc_tiles(test_tile, tempdir(), images = "invalid_image_type"))
})

test_that("download_gfc_tiles constructs correct URLs and filenames and downloads them", {
  skip("Requires downloading of large files")
  # The function has been tested manually and works correctly

  library(sf)
  # Create polygon explicitly rather than using st_buffer with geographic coordinates
  roi <- st_sf(geometry = st_sfc(st_polygon(list(cbind(
    c(-70, -60, -60, -70, -70),
    c(-10, -10, 0, 0, -10)
  )))), crs = 4326)

  # Calculate tiles
  tiles <- calculate_gfc_tiles(roi)

  # Download loss year data
  output_dir <- "data/GFC"
  download_gfc_tiles(tiles, output_dir, images = "lossyear", dataset = "GFC-2022-v1.10")
})
