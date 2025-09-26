#' Download and process GEDI L4B gridded biomass data
#'
#' Downloads, processes, and saves GEDI L4B Gridded Biomass data for a specified region of interest.
#'   The function retrieves data via the ORNL DAAC STAC API, filters for a specified band,
#'   creates a data cube, and saves the result as a GeoTIFF.
#'
#' @param roi An sf object representing the region of interest. If NULL, the global extent of available GEDI L4B data is used.
#' @param gedi_l4b_folder Character, the folder to save the downloaded GeoTIFF file. Default is "data/GEDI_L4B/".
#' @param collection Character, the STAC collection ID for GEDI L4B data.
#'   Default is "GEDI_L4B_Gridded_Biomass_V2_1_2299_2.1".
#' @param gedi_l4b_resolution Numeric, the spatial resolution of the processed output GeoTIFF in degrees. The native resolution of the GEDI L4B gridded dataset is 1 km,
#' approximately 0.001 degrees at the equator. Default is 0.001.
#' @param gedi_l4b_band Character, the band to filter for. See options in the Details section below. Default is "MU".
#'
#' @return Character, the file path to the processed GeoTIFF file.
#'
#' @details
#' This function requires a valid Earthdata Login account and the corresponding
#' credentials Ensure that you have set up your Earthdata Login credentials
#' by either:
#'
#' \itemize{
#'   \item{Setting the EDL_USER and EDL_PASS environment variables in a .Renviron file.}
#'   \item{Running `earthdatalogin::edl_netrc(username = EDL_USER, password = EDL_PASS)`
#'         with your Earthdata Login username and password.}
#' }
#' The available bands are based on GEDI L4B Gridded Biomass Density Version 2.1 documentation:
#'
#'   \itemize{
#'     \item{"MU": Mean aboveground biomass density (MU): Estimated mean
#'     AGBD for the grid cell, including forest and non-forest}
#'     \item{"SE": Mean aboveground biomass density standard error (SE):
#'     Standard Error of the mean estimate, combining sampling and
#'     modeling uncertainty}
#'     \item{"PE": Standard error as a fraction of the estimated mean AGBD.
#'     If >100%, the cell values are truncated to 100.}
#'     \item{"V1": Variance component 1 (V1)}
#'     \item{"V2": Variance component 2 (V2)}
#'     \item{"QF": Quality flag}
#'     \item{"NS": Number of samples}
#'     \item{"MI": Mode of interference}
#'     \item{"PS": Prediction stratum}
#'     \item{"NC": Number of clusters}
#'   }
#'
#'   Consult the reference below for more details about the bands.
#'
# earthdatalogin, rstac, and gdalcubes functions are called with :: notation since they're in Suggests
#' @importFrom sf st_bbox st_as_sf
#' @importFrom terra rast values global plot
#'
#' @references [Dubayah, R., et al. (2022). GEDI L4B Gridded Biomass Data, Version 2.1. NASA Earthdata.](https://doi.org/10.3334/ORNLDAAC/2299)
#'
#' @examples
#' \dontrun{
#'   # Example usage with a defined ROI:
#'   # Define ROI over the Amazon Basin
#'   roi <- sf::st_as_sf(data.frame(y = c(-10, 0), x = c(-70, -60)), coords = c("x", "y"), crs = 4326)
#'   # Downloads biomass data for the defined region of interest
#'   output_file <- download_gedi_l4b(roi = roi, gedi_l4b_band = "MU")
#'   print(output_file)
#' }
download_gedi_l4b <- function(roi = NULL, gedi_l4b_folder = "data/GEDI_L4B/",
                              collection = "GEDI_L4B_Gridded_Biomass_V2_1_2299_2.1",
                              gedi_l4b_resolution = 0.001, gedi_l4b_band = "MU") {

  # Check required packages
  required_pkgs <- c("earthdatalogin", "sf", "gdalcubes", "rstac", "terra")
  for (pkg in required_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Please install package: ", pkg)
    }
  }

  if (is.null(roi)){
    roi <- sf::st_as_sf(data.frame(x = c(-180, 180), y = c(-52, 52)), coords = c("x", "y"), crs = 4326)
  }

  # Define the time range for GEDI L4B data
  time_range <- c("2019-04-18", "2023-03-16")

  # Check if earthdatalogin authentication is set up
  if (!file.exists("~/.netrc")) {
    earthdatalogin::edl_netrc(username = Sys.getenv("EDL_USER"),
                              password = Sys.getenv("EDL_PASS"))
  }

  if (is.null(Sys.getenv("EDL_USER")) & is.null(Sys.getenv("EDL_PASS"))) {
    stop("Earth Data Login credentials not found. Please enter your credentials first by running edl_netrc(username, password) or set a .Renviron file with EDL_USER and EDL_PASS arguments. If needed register your username and password first at https://urs.earthdata.nasa.gov/ \n")
  }

  # Get bounding box from ROI
  bbox <- sf::st_bbox(roi)

  # Query STAC API
  message("Querying STAC API for collection: ", collection, " for band: ", gedi_l4b_band)
  stac_obj <- rstac::stac("https://cmr.earthdata.nasa.gov/stac/ORNL_CLOUD") |>
    rstac::stac_search(
      collections = collection,
      bbox = c(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])
    ) |>
    rstac::post_request() |>
    rstac::items_fetch()


  if (length(stac_obj$features) == 0) {
    stop("No STAC items found for the specified region and time range.")
  }

  # Filter for the specified raster based on the `id` field
  filt_features <- Filter(function(feature) grepl(gedi_l4b_band, feature$id), stac_obj$features)

  if (length(filt_features) == 0) {
    stop("No '", gedi_l4b_band, "' raster assets found in the STAC items.")
  }

  # Dynamically get the asset name:
  asset_name <- names(filt_features[[1]]$assets)[1]

  # Dynamically fetch the href using the asset name
  href <- filt_features[[1]]$assets[[asset_name]]$href

  # srs not in stac meta, so manually set instead
  r <- terra::rast(href, vsi=TRUE)
  srs <- terra::crs(r)

  col <- gdalcubes::stac_image_collection(
    filt_features,
    asset_names = asset_name,
    skip_image_metadata = TRUE,
    srs = srs
  )

  # Create cube view
  cube_view <- gdalcubes::cube_view(
    extent = list(
      t0 = time_range[1],
      t1 = time_range[1],  # Using the same date to handle the single time point.
      left = bbox["xmin"],
      right = bbox["xmax"],
      bottom = bbox["ymin"],
      top = bbox["ymax"]
    ),
    dx = gedi_l4b_resolution,
    dy = gedi_l4b_resolution,
    nt = 1,  # Ensure only one time point is used
    srs = "EPSG:4326"
  )

  # Ensure output directory exists
  if (!dir.exists(gedi_l4b_folder)) {
    dir.create(gedi_l4b_folder, recursive = TRUE)
    message(paste("Created output directory:", gedi_l4b_folder))
  }

  # Generate a prefix for the output files based on band
  prefix <- paste0("GEDI_L4B_", gedi_l4b_band, "_")

  # Write data to GeoTIFF files
  message("Writing data to GeoTIFF file(s)...")

  raster_cube_obj <- gdalcubes::raster_cube(col, cube_view)

  # Check if the raster cube object is empty
  if (length(raster_cube_obj) == 0) {
    stop("The raster cube object is empty.")
  }

  gdalcubes::write_tif(
    raster_cube_obj,
    dir = gedi_l4b_folder,
    prefix = prefix
    #creation_options=c("COMPRESS=DEFLATE")
  )

  # List all created .tif files
  tif_files <- list.files(gedi_l4b_folder, pattern = paste0("^", prefix, ".*\\.tif$"), full.names = TRUE)

  if (length(tif_files) == 0) {
    warning("No .tif files were created.")
    return(character(0))
  }

  # Test the output GeoTIFF files
  message("Testing output GeoTIFF files...")
  t <- terra::rast(tif_files)

  # Check if the raster contains only NA values
  if (all(is.na(terra::values(t)))) {
    stop("The raster contains only NA values. The data may be empty or invalid.")
  } else {
    message("Raster contains valid data.")
    #terra::plot(t, main = paste("GEDI L4B Biomass", gedi_l4b_band))
    #summary_stats <- terra::global(t, fun = "mean", na.rm = TRUE)
    #message("Mean value within ROI: ", summary_stats$mean)
  }

  message("Successfully processed data to: ", gedi_l4b_folder)
  return(tif_files)
}


