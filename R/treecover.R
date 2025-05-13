
#' Compute tree cover forest mask for a specific year
#'
#' This function computes a tree cover forest mask layer for a given year based on a 2000 tree cover baseline - the Hansen Global Forest Change (GFC) dataset
#' or a 2010 tree cover baseline - the GLAD 2010 Tree Canopy Cover dataset.
#'
#' @param year Integer. The year for which to compute the tree cover (2001-2023).
#' @param gfc_folder Character. Path to the folder containing GFC dataset files. Default is "data/GFC".
#' @param treecover_threshold Numeric. Optional threshold for tree cover (0-100) at baseline year. If provided, only areas with
#'        tree cover values >= this threshold will be considered. Default is NULL (no threshold).
#' @param coords Character. Coordinates of the tile to process, e.g., "10N_020E". If NULL, processes all available tiles.
#' @param output_folder Character. Path to the folder where output rasters should be saved.
#' @param num_cores Integer. Number of cores to use for parallel processing.
#' @param baseline Integer. Baseline year for tree cover dataset. Can be 2010 (default) or 2000.
#'
#' @return A list. If coords is specified, only one tile is processed and returns a file path to a single raster.
#'         Otherwise, returns a set of file paths, each representing the tree cover at baseline for the specified year for different tiles.
#'         The rasters are computed taking the tree cover values from the baseline rasters, removing pixels where forest loss was recorded at or before the
#'         provided year. If a tree cover threshold is provided, all tree cover pixels below the threshold are set to NA. Rasters are saved as a GeoTIFF file
#'         saved in the output_folder.
#'
#' @importFrom terra rast crop ext global intersect extract res vect values writeRaster
#' @importFrom stringr str_extract str_match
#' @importFrom parallel makeCluster stopCluster
#' @importFrom pbapply pbsapply
#'
#' @details
#' This function computes a tree cover forest mask by leveraging a baseline tree cover and forest loss data.
#' It uses two primary types of input files:
#' \enumerate{
#'   \item Tree cover data, which serves as the baseline:
#'     \itemize{
#'       \item For the 2000 baseline, the function uses files from the Hansen dataset for tree cover.
#'       \item For the 2010 baseline, the function uses GLAD tree cover files.
#'       \item Tree cover values represent the percentage of tree cover (0-100\%).
#'     }
#'   \item Forest loss year data:
#'     \itemize{
#'       \item The function uses Hansen Global Forest Change lossyear files.
#'       \item Values in these files indicate the year of forest loss (1-23, corresponding to years 2001-2023), or 0 if no loss occurred.
#'     }
#' }
#' The function applies the following steps:
#' \enumerate{
#'   \item It reads the tree cover and forest loss rasters for the specified year and location.
#'   \item If a `treecover_threshold` is provided, all tree cover pixels with values below the threshold are set to NA.
#'   \item It removes pixels where forest loss was recorded at or before the specified year.
#'   \item The resulting raster represents a tree cover forest mask for the specified year, with areas of low tree cover or forest loss masked out.
#'   \item The function saves the resulting raster as a GeoTIFF file in the `output_folder`.
#' }
#'
#' @references Hansen, M.C., Potapov, P.V., Moore, R., Hancher, M., Turubanova, S.A., Tyukavina, A., Thau, D., Stehman, S.V., Goetz, S.J., Loveland,
#'  T.R., Kommareddy, A., Egorov, A., Chini, L., Justice, C.O., and Townshend, J.R.G., 2013, High-Resolution Global Maps of 21st-Century Forest Cover
#'   Change: Science, v. 342, no. 6160, p. 850-853, at http://www.sciencemag.org/content/342/6160/850.abstract.
#'
#' @examples
#' \dontrun{
#' # Compute all tiles tree cover for 2015 using 2010 baseline
#' treecover_2015 <- compute_treecover(2015)
#'
#' # Compute 30% thresholded tree cover for the 10N, 20E tile for 2005 using
#' # 2000 tree cover baseline
#' treecover_2020 <- compute_treecover(2020,
#'                                     treecover_threshold = 30,
#'                                     coords = "10N_020E",
#'                                     output_folder = "data/treecover_calc",
#'                                     baseline = 2000)
#' }
#' @export
compute_treecover <- function(year, gfc_folder = "data/GFC", treecover_threshold = NULL,
                              coords = NULL, output_folder = "data/treecover_calc", num_cores = 1, baseline = 2010) {
  if (year < 2001 || year > 2023) {
    stop("Year must be between 2001 and 2023.")
  }

  if (baseline != 2000 && baseline != 2010) {
    stop("Baseline must be either 2000 or 2010.")
  }

  # Check if output_folder exists and create it if it doesn't
  if (!is.null(output_folder) && !dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
    message(paste("Created output folder:", output_folder))
  }

  treecover_pattern <- if(baseline == 2000) "treecover2000.*\\.tif$" else "treecover2010.*\\.tif$"
  treecover_files <- list.files(gfc_folder, pattern = treecover_pattern, full.names = TRUE)
  lossyear_files <- list.files(gfc_folder, pattern = "lossyear.*\\.tif$", full.names = TRUE)

  if (!is.null(coords)) {
    treecover_files <- treecover_files[grep(coords, treecover_files)]
    lossyear_files <- lossyear_files[grep(coords, lossyear_files)]
    if (length(treecover_files) == 0 || length(lossyear_files) == 0) {
      stop("No matching files found for the specified coordinates.")
    }
  }

  extract_info <- function(filename) {
    if (baseline == 2000) {
      year_version <- str_extract(filename, "GFC-\\d{4}-v\\d+\\.\\d+")
    } else {
      year_version <- "TCC-2010"
    }
    coords <- str_extract(filename, "\\d{2}[NS]_\\d{3}[EW]")
    list(year_version = year_version, coords = coords)
  }

  treecover_info <- lapply(treecover_files, extract_info)
  lossyear_info <- lapply(lossyear_files, extract_info)

  create_identifier <- function(info) {
    paste(info$year_version, info$coords, sep = "_")
  }

  treecover_ids <- sapply(treecover_info, create_identifier)
  lossyear_ids <- sapply(lossyear_info, create_identifier)

  common_ids <- intersect(treecover_ids, lossyear_ids)

  cl <- makeCluster(num_cores)
  on.exit(stopCluster(cl))

  clusterExport(cl, c("process_pair", "year", "treecover_threshold",
                      "output_folder", "treecover_files", "lossyear_files",
                      "treecover_ids", "lossyear_ids", "baseline"),
                envir = environment())

  clusterEvalQ(cl, {
    # Import packages using :: notation instead of library()
    requireNamespace("terra", quietly = TRUE)
    requireNamespace("stringr", quietly = TRUE)
  })

  result_rasters <- pbapply::pblapply(common_ids, function(id) {
    process_pair(id, year, treecover_threshold, output_folder,
                 treecover_files, lossyear_files, treecover_ids, lossyear_ids, baseline)
  }, cl = cl)

  names(result_rasters) <- common_ids
  return(result_rasters)
}



#' Process a single pair of tree cover and forest loss files
#'
#' @param id The identifier for the file pair
#' @param year The year for which to compute tree cover
#' @param treecover_threshold The threshold for tree cover
#' @param output_folder The folder to save output rasters
#' @param treecover_files List of tree cover files
#' @param lossyear_files List of forest loss files
#' @param treecover_ids IDs for tree cover files
#' @param lossyear_ids IDs for forest loss files
#' @param baseline Baseline year for tree cover dataset (2000 or 2010)
#'
#' @return A file path to a geotiff raster representing the computed tree cover
process_pair <- function(id, year, treecover_threshold, output_folder,
                         treecover_files, lossyear_files, treecover_ids, lossyear_ids, baseline) {

  # Load baseline tree cover and forest loss rasters
  treecover_file <- treecover_files[treecover_ids == id]
  lossyear_file <- lossyear_files[lossyear_ids == id]

  # Check if files exist
  if (length(treecover_file) == 0) {
    stop("No tree cover file found for ID: ", id)
  }
  if (length(lossyear_file) == 0) {
    stop("No forest loss file found for ID: ", id)
  }

  # Check if files can be read
  if (!file.exists(treecover_file)) {
    stop("Tree cover file does not exist: ", treecover_file)
  }
  if (!file.exists(lossyear_file)) {
    stop("Forest loss file does not exist: ", lossyear_file)
  }

  # Load rasters with error handling
  tryCatch({
    treecover_baseline <- rast(treecover_file)
  }, error = function(e) {
    stop("Failed to read tree cover file: ", treecover_file, "\nError: ", e$message)
  })

  tryCatch({
    forest_loss <- rast(lossyear_file)
  }, error = function(e) {
    stop("Failed to read forest loss file: ", lossyear_file, "\nError: ", e$message)
  })

  # Ensure rasters have the same extent and resolution
  if (!compareGeom(treecover_baseline, forest_loss, stopOnError = FALSE)) {
    forest_loss <- resample(forest_loss, treecover_baseline)
  }

  # Convert year to two-digit format for comparison with forest loss raster
  year_2digit <- year %% 100

  # Apply threshold to baseline tree cover (if specified)
  if (!is.null(treecover_threshold)) {
    # Explicitly set values below the threshold to NA
    treecover_baseline[treecover_baseline < treecover_threshold] <- NA
  }

  # Create a binary mask for forest loss up to the specified year
  forest_loss_binary <- forest_loss > 0 & forest_loss <= year_2digit

  # Subtract forest loss from baseline tree cover
  treecover_y <- mask(treecover_baseline, !forest_loss_binary)

  # Save output raster to disk
  if (!is.null(output_folder)) {
    if (!dir.exists(output_folder)) {
      dir.create(output_folder, recursive = TRUE)
    }
    original_filename <- basename(treecover_file)
    filename_parts <- strsplit(original_filename, "_")[[1]]
    new_filename <- paste0(
      ifelse(baseline == 2000, "Hansen_", "GLAD_"),
      ifelse(baseline == 2000, filename_parts[2], "TCC-2010"),
      "_treecover_calc_", year, "_",
      filename_parts[length(filename_parts)-1], "_", filename_parts[length(filename_parts)]
    )
    output_path <- file.path(output_folder, new_filename)
    writeRaster(treecover_y, output_path, overwrite = TRUE)

    return(output_path)
  } else {
    # If no output folder specified, return the raster itself
    return(treecover_y)
  }
}


#' Download Global 2010 Tree Cover 30m data
#'
#' This function downloads the Global 2010 Tree Cover 30m data within a specified region of interest
#'  from the GLAD lab at the University of Maryland.
#'
#' @param roi An sf object representing the region of interest. If NULL, the global extent is used.
#' @param output_folder Directory to save downloaded files. Default is "data/GLAD_TCC_2010".
#' @param n_cores Number of cores to use for parallel download. Default is 1.
#' @param timeout Number of seconds for reaching file download timeout. Default is 1800.
#'
#' @return A character vector of downloaded file paths.
#'
#' @import sf
#' @import httr
#' @import rvest
#' @import pbapply
#' @importFrom sf st_crs st_transform st_intersects st_convex_hull st_touches
#'
#' @export
#'
#' @references [Hansen, M. C., Potapov, P. V., Moore, R., Hancher, M., Turubanova, S. A., Tyukavina, A., ... & Townshend, J. R. G. (2013). High-resolution global maps of 21st-century forest cover change. science, 342(6160), 850-853.](https://doi.org/10.1126/science.1244693)
#'
#' @examples
#' \dontrun{
#' roi <- st_as_sf(data.frame(x = c(-70, -60), y = c(-10, 0)), coords = c("x", "y"), crs = 4326)
#' download_glad_tcc_2010(roi = roi)
#'}
#'
download_glad_tcc_2010 <- function(roi = NULL,
                                   output_folder = "data/GLAD_TCC_2010",
                                   n_cores = 1,
                                   timeout = 1800) {

  base_url <- "https://glad.umd.edu/Potapov/TCC_2010/"

  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
    message(paste("Created output directory:", output_folder))
  }

  page <- rvest::read_html(base_url)
  file_links <- rvest::html_attr(rvest::html_nodes(page, "a"), "href")
  tile_list <- grep("^treecover2010_.*\\.tif$", file_links, value = TRUE)


  if (!is.null(roi)) {
    bbox <- sf::st_bbox(roi)

    intersects_roi <- function(tile) {
      tryCatch({
        # Remove prefix and suffix
        tile <- gsub("treecover2010_", "", tile)
        tile <- gsub(".tif", "", tile)

        # Extract latitude and longitude using regex
        lat <- as.numeric(sub("([0-9]+)([NS]).*", "\\1", tile)) * ifelse(grepl("N", tile), 1, -1)
        lon <- as.numeric(sub(".*_([0-9]+)([EW])", "\\1", tile)) * ifelse(grepl("E", tile), 1, -1)

        # Define bounding box for the tile
        tile_bbox <- c(xmin = lon, ymin = lat, xmax = lon + 10, ymax = lat + 10)

        # Check if the ROI intersects with the tile's bounding box
        return(bbox["xmin"] < tile_bbox["xmax"] && bbox["xmax"] > tile_bbox["xmin"] &&
                 bbox["ymin"] < tile_bbox["ymax"] && bbox["ymax"] > tile_bbox["ymin"])
      }, error = function(e) {
        return(FALSE)  # Skip this tile if any error occurs during processing
      })
    }

    tile_list <- tile_list[sapply(tile_list, intersects_roi)]
  }

  download_tile <- function(tile) {
    options(timeout = max(timeout, getOption("timeout")))

    file_url <- paste0(base_url, tile)
    output_file <- file.path(output_folder, tile)

    tryCatch({
      utils::download.file(file_url, output_file, mode = "wb", quiet = TRUE)
      return(output_file)
    }, error = function(e) {
      warning(paste("Failed to download:", tile, "-", e$message))
      return(NULL)
    })
  }

  message(paste0("Downloading ", length(tile_list), " GLAD Tree Cover 2010 tile(s)..."))

  downloaded_files <- pbapply::pblapply(tile_list, download_tile, cl = n_cores)

  downloaded_files <- unlist(downloaded_files)
  downloaded_files <- downloaded_files[!sapply(downloaded_files, is.null)]

  return(downloaded_files)
}




#' Verify downloaded GFC tile file integrity
#'
#' Checks if the downloaded file size matches the remote file size.
#'
#' @param tile_url URL of the remote file
#' @param local_path Path to the local downloaded file
#'
#' @return Integer: 0 if verification passed, 3 if failed
#'
#' @importFrom httr HEAD
#' @importFrom stringr str_extract
#'
#' @keywords internal
verify_download <- function(tile_url, local_path) {
  # Get headers from the URL using httr
  response <- httr::HEAD(tile_url)
  headers <- httr::headers(response)

  # Extract content length
  remote_size <- as.numeric(headers$`content-length`)
  local_size <- file.info(local_path)$size

  # Compare sizes
  if (is.na(remote_size) || remote_size != local_size) {
    return(3)
  } else {
    return(0)
  }
}

#' Download a single GFC tile
#'
#' Downloads a single GFC tile and verifies the download.
#'
#' @param tile_url URL of the tile to download
#' @param local_path Local path to save the tile
#' @param timeout Timeout in seconds for download
#'
#' @return Integer: 0 if successful, 1 if download failed, 2 if verification failed
#'
#' @importFrom utils download.file
#'
#' @keywords internal
download_tile <- function(tile_url, local_path, timeout = 1000) {
  options(timeout=timeout)
  tryCatch({
    ret_code <- download.file(tile_url, local_path, mode="wb", quiet = FALSE)
    if (ret_code != 0) {
      message(paste('Warning: problem downloading', basename(local_path)))
      return(1)
    } else if (verify_download(tile_url, local_path) != 0) {
      message(paste("Warning: verification failed on", basename(local_path)))
      return(2)
    } else {
      message(paste("Successfully downloaded", basename(local_path)))
      return(0)
    }
  }, error = function(e) {
    message(paste('Error downloading', basename(local_path), ':', e$message))
    return(1)
  })
}

#' Create a global grid of GFC tiles
#'
#' Creates a global grid of 10x10 degree tiles covering the world,
#' matching the Hansen Global Forest Change product naming convention.
#'
#' @return An \code{sf} object representing the global grid of GFC tiles.
#'
#' @importFrom sf st_polygon st_sfc st_sf
#'
#' @keywords internal
create_gfc_tiles_grid <- function() {
    # Create a global 10x10 degree grid
    lons <- seq(-180, 170, by=10)
    lats <- seq(-90, 80, by=10)  # Include full range to -90

    # Create an empty list to store the polygons
    polys <- list()
    tile_names <- character()

    # Create a polygon for each tile and assign tile ID
    for (lon in lons) {
        for (lat in lats) {
            # Skip areas outside Hansen coverage (south of -60°)
            if (lat < -60) next

            # Create the polygon coordinates - the polygon represents the area covered by the tile
            poly_coords <- matrix(c(
                lon, lat,
                lon+10, lat,
                lon+10, lat+10,
                lon, lat+10,
                lon, lat
            ), ncol=2, byrow=TRUE)

            # Create the polygon
            poly <- sf::st_polygon(list(poly_coords))

            # Generate tile name using the correct Hansen GFC naming convention
            # For a polygon spanning lat to lat+10 and lon to lon+10,
            # the correct tile name is (lat+10)N_lon - this represents the top-left corner

            # Handle special cases for latitude naming according to Hansen GFC convention
            north_lat <- lat + 10  # The northern (top) edge of the tile

            # Hansen GFC naming convention edge cases:
            # 1. Tiles are named after their top-left corner
            # 2. The equatorial tile (0 to -10) is named "00N" not "10S"
            # 3. For all other tiles, they use the latitude of the northern edge with the sign
            if (north_lat == 0) {
                lat_str <- "00N"  # Equatorial North
            } else if (north_lat > 0) {
                lat_str <- sprintf("%02dN", north_lat)  # Northern hemisphere
            } else if (north_lat == -10 && lat == -10) {
                # Special case: equatorial tile (0 to -10)
                lat_str <- "00N"
            } else {
                lat_str <- sprintf("%02dS", abs(north_lat))  # Southern hemisphere
            }

            # Handle longitude - we want the western (left) edge of the tile
            # Special case for -180/180 boundary (use 180W consistently)
            if (lon == 180 || lon == -180) {
                lon_str <- "180W"
            } else if (lon >= 0) {
                lon_str <- sprintf("%03dE", lon)
            } else {
                lon_str <- sprintf("%03dW", abs(lon))
            }

            # Combine to form tile ID
            tile_id <- paste0(lat_str, "_", lon_str)

            # Add to lists
            polys <- c(polys, list(poly))
            tile_names <- c(tile_names, tile_id)
        }
    }

    # Create an sf object from the polygons
    grid_sfc <- sf::st_sfc(polys, crs=4326)
    grid_sf <- sf::st_sf(geometry=grid_sfc, tile_id=tile_names)

    return(grid_sf)
}

#' Calculate the GFC product tiles needed for a given area
#'
#' Determines which Global Forest Change (GFC) product tiles intersect
#' with a specified area of interest.
#'
#' @param aoi An sf object representing the Area of Interest
#' @param recreate_grid Logical. If TRUE, force recreation of the GFC tiles grid
#'
#' @return A filtered sf object containing only the tiles that intersect with the AOI
#'
#' @importFrom sf st_crs st_transform st_intersects st_bbox st_as_sfc
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Create a polygon in Central Africa
#' aoi <- st_sf(geometry = st_sfc(st_polygon(list(cbind(
#'   c(20, 21, 21, 20, 20),
#'   c(-1, -1, 1, 1, -1)))), crs = 4326))
#'
#' tiles <- calculate_gfc_tiles(aoi)
#' }
calculate_gfc_tiles <- function(aoi, recreate_grid = FALSE) {
    # Validate input
    if (!inherits(aoi, "sf")) {
        stop("aoi must be an sf object")
    }

    # Create or retrieve the GFC tiles grid
    if (!exists("gfc_tiles_grid", envir = .GlobalEnv) || recreate_grid) {
        # Create the global tile grid
        gfc_tiles_grid <- create_gfc_tiles_grid()
        assign("gfc_tiles_grid", gfc_tiles_grid, envir = .GlobalEnv)
    } else {
        gfc_tiles_grid <- get("gfc_tiles_grid", envir = .GlobalEnv)
    }

    # Ensure the AOI is in WGS84
    if (!identical(st_crs(aoi), st_crs(4326))) {
        warning("AOI does not have WGS84 CRS. Reprojecting for tile calculation.")
        aoi <- st_transform(aoi, 4326)
    }

    # Instead of using st_convex_hull which can increase the AOI's extent,
    # directly find tiles that intersect with the AOI geometry
    intersects <- st_intersects(gfc_tiles_grid, aoi)

    # Find tiles that intersect with the AOI
    intersecting_idx <- which(lengths(intersects) > 0)

    if (length(intersecting_idx) == 0) {
        stop("No GFC tiles intersect with the provided AOI")
    }

    # Return the intersecting tiles
    return(gfc_tiles_grid[intersecting_idx, ])
}

#' Download Global Forest Change (GFC) product tiles
#'
#' Downloads the specified GFC product tiles from the Hansen et al. Global Forest Change dataset.
#'
#' @param tiles An sf object with GFC tiles information as returned by \code{calculate_gfc_tiles}
#' @param output_dir Directory where downloaded files will be saved
#' @param images Character vector specifying which image types to download.
#'        Options include 'treecover2000', 'lossyear', 'gain', 'datamask', 'first', and 'last'
#' @param dataset Version of the Hansen GFC dataset to use (e.g., 'GFC-2023-v1.11')
#' @param timeout Download timeout in seconds
#'
#' @return Character vector of paths to successfully downloaded files
#'
#' @importFrom utils download.file
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Create a polygon in Central Africa
#' aoi <- st_sf(geometry = st_sfc(st_polygon(list(cbind(
#'   c(20, 21, 21, 20, 20),
#'   c(-1, -1, 1, 1, -1)))), crs = 4326))
#'
#' # Calculate required tiles
#' tiles <- calculate_gfc_tiles(aoi)
#'
#' # Download tree cover and loss year data
#' download_gfc_tiles(
#'   tiles,
#'   "data/GFC",
#'   images = c("treecover2000", "lossyear"),
#'   dataset = "GFC-2023-v1.11"
#' )
#' }
download_gfc_tiles <- function(tiles, output_dir,
                              images = c("treecover2000", "lossyear"),
                              dataset = "GFC-2023-v1.11",
                              timeout = 1000) {
    # Validate input
    if (!inherits(tiles, "sf")) {
        stop("tiles must be an sf object")
    }

    valid_images <- c("treecover2000", "lossyear", "gain", "datamask", "first", "last")
    invalid_images <- setdiff(images, valid_images)
    if (length(invalid_images) > 0) {
        stop(paste("Invalid image types:", paste(invalid_images, collapse=", "),
                  "\nValid options are:", paste(valid_images, collapse=", ")))
    }

    # Create output directory if needed
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
        message(paste("Created output directory:", output_dir))
    }

    # Base URL for Hansen GFC dataset
    base_url <- "https://storage.googleapis.com/earthenginepartners-hansen"

    # Track download statistics
    downloaded_files <- character()

    # Process each tile
    for (i in 1:nrow(tiles)) {
        tile_id <- tiles$tile_id[i]

        # Process each image type for this tile
        for (img_type in images) {
            # Construct the remote URL and local file path
            file_url <- paste0(base_url, "/", dataset, "/Hansen_", dataset, "_", img_type, "_", tile_id, ".tif")
            local_path <- file.path(output_dir, paste0("Hansen_", dataset, "_", img_type, "_", tile_id, ".tif"))

            # Skip if file exists and is verified
            if (file.exists(local_path)) {
                if (verify_download(file_url, local_path) == 0) {
                    message(paste(basename(local_path), "already exists and verified - skipping"))
                    downloaded_files <- c(downloaded_files, local_path)
                    next
                } else {
                    message(paste(basename(local_path), "exists but verification failed - re-downloading"))
                }
            }

            # Download the file
            message(paste("Downloading", file_url, "to", local_path))
            download_result <- download_tile(file_url, local_path, timeout)

            if (download_result == 0) {
                downloaded_files <- c(downloaded_files, local_path)
            } else {
                warning(paste("Failed to download", file_url, "- will continue processing other tiles"))
            }
        }
    }

    return(downloaded_files)
}

