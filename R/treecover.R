
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
    library(terra)
    library(stringr)
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

  treecover_baseline <- rast(treecover_file)
  forest_loss <- rast(lossyear_file)

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




### Benchmarks:
# The main gains with parallelism are when n_cores = nr of tiles to compute as seen below:

# library(microbenchmark)
#
## Two tiles
# compute_1core <- function() {
#   compute_treecover(year = 2015, num_cores = 1)
# }
#
# compute_2cores <- function() {
#   compute_treecover(year = 2015, num_cores = 2)
# }
#
# microbenchmark(
#   "1 core" = compute_1core(),
#   "2 cores" = compute_2cores(),
#   times = 1
# )
# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# 1 core 1406.839 1406.839 1406.839 1406.839 1406.839 1406.839     1
# 2 cores 1026.846 1026.846 1026.846 1026.846 1026.846 1026.846     1

## Single tile
# compute_1core <- function() {
#   compute_treecover(2020, treecover_threshold = 30, coords = "50N_010W", output_folder = "data/treecover_calc", num_cores = 1)
# }
#
# compute_20cores <- function() {
#   compute_treecover(2020, treecover_threshold = 30, coords = "50N_010W", output_folder = "data/treecover_calc", num_cores = 20)
# }
#
# microbenchmark(
#   "1 core" = compute_1core(),
#   "20 cores" = compute_20cores(),
#   times = 1
# )
# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# 1 core 1314.738 1314.738 1314.738 1314.738 1314.738 1314.738     1
# 20 cores 1083.504 1083.504 1083.504 1083.504 1083.504 1083.504     1
