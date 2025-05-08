## Updates made to the new framework:
# 16/01/2025:
# Used sf and terra packages consistently for spatial operations.
# Improved error handling with tryCatch.
# Ensured compatibility with both sf and terra input types.
# Removed dependencies on non-CRAN packages.
# 01/02/2025:
# Automatic donwload of ESA CCI Biomass tile data if not available locally
# Increased 25x the speed in the weighted mean case calculation (with no parallelism yet)
# 03/02/2025:
# Added weighted_mean = TRUE case when agb_raster or forest_mask is not NULL for both sampleAGBmap and sampleTreeCover.
# 27/02/25:
# Added a new source parameter to specify whether to use ESA CCI BIOMASS or GEDI L4B data. When source = "gedi",
# the function calls download_gedi_l4b to retrieve the GEDI L4B data before processing it.
# The gedi_l4b_folder and gedi_l4b_band parameters allow users to specify the download folder and the desired GEDI L4B band.
# 09/05/2025:
# Fixed critical bug in sampleTreeCover where terra::extract() accessed the ID column instead of raster values.
# Used ID=FALSE parameter to ensure consistent extraction behavior across terra package versions.

## Notes:
# `AGBown` and `own` arguments are redundant, if there's no problem with not breaking old legacy code, `own` argument shall be removed.
# sampleAGBmap was re-written because it was broken. For this reason we cannot fully compare old vs new functions, so we need a reproducible example with expected output result to validate old vs new functions.
# Do we only calculate mean AGB here, no need for SD for uncertainty propagation?




#' Sample block mean mapped AGB over a region of interest
#'
#' This function samples the block mean mapped Above Ground Biomass (AGB) over a given polygon.
#' It can use either a custom AGB map provided as input, download and use ESA CCI BIOMASS AGB tiles,
#' or download and use GEDI L4B Gridded Biomass data.
#'
#' @param roi An sf or SpatVector object representing the Region of Interest.
#' @param weighted_mean Logical, if TRUE the weighted mean is calculated considering the
#'  approximate fraction of each cell that is covered by the roi (default is FALSE).
#' @param dataset Character, the dataset to use for AGB estimation. Options are "custom", "esacci", or "gedi". Default is "custom".
#' @param agb_raster A SpatRaster object with the custom AGB map. If NULL, either ESA CCI BIOMASS AGB tiles or GEDI L4B data will be downloaded and used.
#' @inheritParams download_esacci_biomass
#' @inheritParams download_gedi_l4b
#'
#' @return Numeric value representing the mean AGB for the polygon.
#'
#' @import parallel pbapply
#' @importFrom sf st_as_sf
#' @importFrom terra rast extract
#'
#' @export
#'
#' @references [Santoro, M.; Cartus, O. (2024): ESA Biomass Climate Change Initiative (Biomass_cci): Global datasets of forest above-ground biomass for the years 2010, 2015, 2016, 2017, 2018, 2019, 2020 and 2021, v5.01. NERC EDS Centre for Environmental Data Analysis, 22 August 2024.](https://dx.doi.org/10.5285/bf535053562141c6bb7ad831f5998d77)
#' @references [Dubayah, R., et al. (2022). GEDI L4B Gridded Biomass Data, Version 2.1. NASA Earthdata.](https://doi.org/10.3334/ORNLDAAC/2299)
#'
#' @examples
#' # Load required libraries
#' library(sf)
#'
#' # Define a region of interest (ROI) in the Congo Rainforest
#' roi_congo <- st_polygon(list(rbind(
#'   c(25.0089, 0.4735), c(25.0189, 0.4735),
#'   c(25.0189, 0.4835), c(25.0089, 0.4835),
#'   c(25.0089, 0.4735)
#' )))
#' roi_sf_congo <- st_sfc(roi_congo, crs = 4326)
#'
#' # Example 1: Calculate mean AGB for the Congo ROI using ESA CCI BIOMASS (unweighted)
#' sampleAGBmap(roi_sf_congo, dataset = "esacci")
#'
#' # Example 2: Calculate mean AGB for the Congo ROI using GEDI L4B (weighted)
#' sampleAGBmap(roi_sf_congo, dataset = "gedi", weighted_mean = TRUE)
#'
sampleAGBmap <- function(
    roi,
    weighted_mean = FALSE,
    agb_raster = NULL,
    dataset = "custom",
    esacci_biomass_year = "latest",
    esacci_biomass_version = "latest",
    esacci_folder = "data/ESACCI-BIOMASS",
    gedi_l4b_folder = "data/GEDI_L4B/",
    gedi_l4b_band = "MU",
    gedi_l4b_resolution = 0.001,
    n_cores = 1,
    timeout = 600
) {

  # Initialize AGB value
  AGB <- NA

  # Use custom AGB raster if provided
  if (!is.null(agb_raster) && dataset == "custom") {
    agb_raster[agb_raster == 0] <- NA

    tryCatch({
      # Extract values with or without weights based on weighted_mean
      extracted_vals <- terra::extract(agb_raster, sf::st_as_sf(roi), weights = weighted_mean, normalizeWeights = FALSE)

      if (!is.null(extracted_vals) && nrow(extracted_vals) > 0) {
        # Remove rows with NA values in the AGB column
        ids <- which(!is.na(extracted_vals[, 2]))
        vls <- extracted_vals[ids, ]

        if (weighted_mean) {
          # Weighted mean case
          if (length(vls) > 2) {
            AGB <- sum(vls[, 2] * vls[, 3]) / sum(vls[, 3])
          }
        } else {
          # Unweighted mean case
          AGB <- mean(vls[, 2], na.rm = TRUE)
        }
      }
    }, error = function(e) {
      message("Error extracting values from agb_raster: ", e$message)
    })
  } else if (dataset == "esacci") {

    # Get ESA CCI AGB raster data:
    esacci_biomass_args <- validate_esacci_biomass_args(esacci_biomass_year, esacci_biomass_version)

    ras <- ESACCIAGBtileNames(roi, esacci_biomass_args$esacci_biomass_year, esacci_biomass_args$esacci_biomass_version)

    AGB_all <- lapply(ras, function(f) {
      # Print the current file being processed
      message("Processing file: ", f)

      # Check if the file already exists
      if (!file.exists(file.path(esacci_folder, f))) {
        message("File not found locally. Attempting to download...")

        # Download the file
        download_esacci_biomass(
          esacci_biomass_year = esacci_biomass_year,
          esacci_biomass_version = esacci_biomass_version,
          esacci_folder = esacci_folder,
          n_cores = n_cores,
          timeout = timeout,
          file_names = f
        )

        # Verify if the file was downloaded successfully
        if (!file.exists(file.path(esacci_folder, f))) {
          stop(paste0("Failed to download ", f, ". Please try again later or check the input arguments."))
        } else {
          message("Download successful: ", f)
        }
      } else {
        message("File already exists locally")
      }

      # Try to extract values from the raster
      tryCatch({
        message("Loading raster")
        raster_obj <- terra::rast(file.path(esacci_folder, f))

        message("Extracting values for ROI...")
        extracted_vals <- terra::extract(raster_obj, sf::st_as_sf(roi), weights = weighted_mean, normalizeWeights = FALSE)
        colnames(extracted_vals)[2] <- "agb"

        message("Extraction complete")
      }, error = function(e) {
        message("Error extracting values from ", f, ": ", e$message)
        extracted_vals <- NULL
      })

      # Extract AGB and, if applicable, the weights
      if (!weighted_mean & (length(extracted_vals) == 2) & length(extracted_vals[, 2]) > 0) {
        # No weighted mean case
        agb_vls <- extracted_vals[, 2]
        message("AGB values extracted (weighted mean = FALSE): ", length(agb_vls), " values")

      } else if (weighted_mean & (length(extracted_vals) == 3) & length(extracted_vals[, 2]) > 0) {
        # Weighted mean case
        agb_vls <- extracted_vals[, 2:3]
        message("AGB values extracted (weighted mean = TRUE): ", nrow(agb_vls), " values")

      } else {
        agb_vls <- NA
        message("No valid AGB values extracted for: ", f)
      }

      # Return the extracted values
      return(agb_vls)
    })

    # Check if all elements are data frames (weighted mean = TRUE) or numeric vectors (weighted mean = FALSE):
    if (all(sapply(AGB_all, is.data.frame))) {
      # Combine all data frames into one
      combined_agb <- dplyr::bind_rows(AGB_all)
    } else if (all(sapply(AGB_all, is.numeric))) {
      # Combine all numeric vectors into one and convert to a data frame
      combined_agb <- data.frame(agb = unlist(AGB_all))
    } else {
      stop("AGB_all must be a list of data frames or a list of numeric vectors.")
    }

    # Calculate mean AGB:
    if (!weighted_mean & (length(combined_agb) == 1) & length(combined_agb$agb) > 0) {
      # No weighted mean case:
      AGB <- mean(combined_agb$agb, na.rm = TRUE)

    } else if (weighted_mean & (length(combined_agb) == 2) & length(combined_agb$agb) > 0) {
      # Weighted mean case:
      cleaned_agb <- na.omit(combined_agb[, 1:2])
      # AGB <- sum(apply(cleaned_agb, 1, prod)) / sum(cleaned_agb[, 2])
      AGB <- sum(cleaned_agb[, 1] * cleaned_agb[, 2]) / sum(cleaned_agb[, 2]) # 25x faster than above line

    } else {
      AGB <- NA
    }

  } else if (dataset == "gedi") {

    # Download and process GEDI L4B data
    gedi_tif_files <- download_gedi_l4b(roi = roi, gedi_l4b_folder = gedi_l4b_folder, gedi_l4b_band = gedi_l4b_band,
                                        gedi_l4b_resolution = gedi_l4b_resolution)

    if (length(gedi_tif_files) == 0) {
      stop("No GEDI L4B data was downloaded or processed.")
    }

    # Load the GEDI L4B raster
    gedi_raster <- terra::rast(gedi_tif_files)

    # Extract values with or without weights based on weighted_mean
    extracted_vals <- terra::extract(gedi_raster, sf::st_as_sf(roi), weights = weighted_mean, normalizeWeights = FALSE)

    if (!is.null(extracted_vals) && nrow(extracted_vals) > 0) {
      # Remove rows with NA values in the AGB column
      ids <- which(!is.na(extracted_vals[, 2]))
      vls <- extracted_vals[ids, ]

      if (weighted_mean) {
        # Weighted mean case
        if (length(vls) > 2) {
          AGB <- sum(vls[, 2] * vls[, 3]) / sum(vls[, 3])
        }
      } else {
        # Unweighted mean case
        AGB <- mean(vls[, 2], na.rm = TRUE)
      }
    }
  } else {
    stop("Invalid dataset option. Please choose 'custom', 'esacci', or 'gedi'.")
  }

  return(AGB)
}




#' Sample block mean mapped forest cover over a region of interest
#'
#' This function samples the block mean mapped forest cover over a given polygon.
#' It can use either a custom forest cover mask provided as input or download and use
#' Global Forest Change (GFC) tree cover tiles (Hansen et al., 2013).
#'
#' @param roi An sf or SpatVector object representing the Region of Interest.
#' @param thresholds Numeric vector of tree cover thresholds percentages (e.g., c(10, 20, 30)) to calculate forest cover percentages.
#' @param weighted_mean Logical, if TRUE the weighted mean is calculated considering the
#'  approximate fraction of each cell that is covered by the roi (default is FALSE).
#' @param forest_mask A SpatRaster object with a custom forest cover mask. If NULL, Hansen GFC tree cover tiles will be downloaded and used.
#' @inheritParams Deforested
#'
#' @return Numeric value representing the mean forest cover for the polygon.
#'
#' @importFrom sf st_as_sf
#' @importFrom terra rast extract
#' @importFrom dplyr bind_rows
#'
#' @export
#'
#' @references M. C. Hansen et al., High-Resolution Global Maps of 21st-Century Forest Cover Change. Science342,850-853(2013). [DOI:10.1126/science.1244693](https://doi.org/10.1126/science.1244693)
#'
#' @examples
#' # Load required libraries
#' library(sf)
#'
#' # Define a region of interest (ROI) in the Daintree forest
#' roi_daintree <- st_polygon(list(rbind(c(145.3833, -16.2500), c(145.3933, -16.2500),
#'                                       c(145.3933, -16.2400), c(145.3833, -16.2400),
#'                                       c(145.3833, -16.2500))))
#' roi_sf_daintree <- st_sfc(roi_daintree, crs = 4326)
#'
#' # Example 1: Calculate forest cover (unweighted)
#' sampleTreeCover(roi_sf_daintree, thresholds = c(10, 20, 30))
#'
#' # Example 2: Calculate forest cover (weighted)
#' sampleTreeCover(roi_sf_daintree, thresholds = c(10, 20, 30), weighted_mean = TRUE)
#'
sampleTreeCover <- function(
    roi,
    thresholds,
    forest_mask = NULL,
    weighted_mean = FALSE,
    gfc_folder = "data/GFC",
    gfc_dataset_year = "latest"
) {

  # Initialize values
  forest_cover <- numeric()

  # Verify gfc_folder exists, create if it doesn't
  if (!dir.exists(gfc_folder)) {
    dir.create(gfc_folder, recursive = TRUE)
    message(paste("Created directory:", gfc_folder))
  }

  if (gfc_dataset_year == "latest") {
    gfc_dataset_year <- 2023
  }

  if (!gfc_dataset_year %in% seq(2023, 2015)) {
    stop("Invalid gfc_dataset_year. Please use a year between 2015 and 2023.")
  }

  dataset_str <- paste0("GFC-", gfc_dataset_year, "-v1.", (gfc_dataset_year - 2018) + 6)

  # Use custom forest mask if provided
  if (!is.null(forest_mask)) {
    tryCatch({
      if (weighted_mean) {
        # Extract values with weights
        extracted_vals <- terra::extract(forest_mask, sf::st_as_sf(roi), weights = TRUE, normalizeWeights = FALSE)
        if (!is.null(extracted_vals) && nrow(extracted_vals) > 0) {
          # Calculate forest cover for each threshold
          for (threshold in thresholds) {
            tmp <- extracted_vals
            tmp[, 2] <- ifelse(tmp[, 2] > threshold, 1.0, 0.0)  # Convert to binary based on threshold
            if (sum(tmp[, 3]) > 0) {
              forest_cover <- c(forest_cover, sum(tmp[, 2] * tmp[, 3]) / sum(tmp[, 3]))
            }
          }
        }
      } else {
        # Unweighted case - Use ID=FALSE to get values directly
        extracted_vals <- terra::extract(forest_mask, sf::st_as_sf(roi), ID=FALSE)[[1]]
        if (!is.null(extracted_vals)) {
          # Calculate forest cover for each threshold
          for (threshold in thresholds) {
            tmp <- ifelse(extracted_vals > threshold, 1.0, 0.0)  # Convert to binary based on threshold
            forest_cover <- c(forest_cover, mean(tmp, na.rm = TRUE))
          }
        }
      }
    }, error = function(e) {
      message("Error extracting values from forest_mask: ", e$message)
    })
  } else {
    # Get Hansen GFC tree cover tile names for the ROI
    gfcTile <- suppressMessages(suppressWarnings(gfcanalysis::calc_gfc_tiles(roi)))
    gfcanalysis::download_tiles(gfcTile, gfc_folder, images = "treecover2000", dataset = dataset_str, timeout = 1000)

    # Get overlapping tile/s (up to 4 possible tiles)
    bb <- sf::st_bbox(roi)
    crds <- expand.grid(x = bb[c(1, 3)], y = bb[c(2, 4)])
    fnms <- character(4)

    for (i in 1:nrow(crds)) {
      lon <- 10 * (crds[i, 1] %/% 10)
      lat <- 10 * (crds[i, 2] %/% 10) + 10
      LtX <- ifelse(lon < 0, "W", "E")
      LtY <- ifelse(lat < 0, "S", "N")
      WE <- paste0(sprintf('%03d', abs(lon)), LtX)
      NS <- paste0(sprintf('%02d', abs(lat)), LtY)

      fnms[i] <- paste0("Hansen_", dataset_str, "_treecover2000_", NS, "_", WE, ".tif")
    }

    # Process each tile
    forest_cover_all <- lapply(unique(fnms), function(f) {
      # Print the current file being processed
      message("Processing tile: ", f)

      # Verify if the file was downloaded successfully
      if (!file.exists(file.path(gfc_folder, f))) {
        stop(paste0("Failed to download ", f, ". Please try again later or check the input arguments."))
      }

      # Try to extract values from the raster
      tryCatch({
        raster_obj <- terra::rast(file.path(gfc_folder, f))

        message("Extracting values for ROI...")
        extracted_vals <- terra::extract(raster_obj, sf::st_as_sf(roi), weights = weighted_mean, normalizeWeights = FALSE)
        colnames(extracted_vals)[2] <- "treecover"

        message("Extraction complete")
      }, error = function(e) {
        message("Error extracting values from ", f, ": ", e$message)
        return(NULL)
      })

      # Return extracted values
      return(extracted_vals)
    })

    # Combine all extracted values
    combined_vals <- dplyr::bind_rows(forest_cover_all)

    # Calculate forest cover for each threshold
    if (!is.null(combined_vals) && nrow(combined_vals) > 0) {
      for (threshold in thresholds) {
        if (weighted_mean) {
          # Weighted mean case
          tmp <- combined_vals
          tmp$treecover <- ifelse(tmp$treecover > threshold, 1.0, 0.0)
          if (sum(tmp$weight) > 0) {
            forest_cover <- c(forest_cover, sum(tmp$treecover * tmp$weight) / sum(tmp$weight))
          }
        } else {
          # Unweighted mean case
          tmp <- combined_vals$treecover
          tmp <- ifelse(tmp > threshold, 1.0, 0.0)
          forest_cover <- c(forest_cover, mean(tmp, na.rm = TRUE))
        }
      }
    }
  }

  return(forest_cover)
}




# sampleTreeCover <- function(
    #     roi,
#     thresholds,
#     forest_mask = NULL,
#     weighted_mean = FALSE,
#     gfc_folder = "data/GFC",
#     gfc_dataset_year = "latest"
# ) {
#   # Initialize values
#   forest_cover <- numeric()
#
#   # Verify gfc_folder exists, create if it doesn't
#   if (!dir.exists(gfc_folder)) {
#     dir.create(gfc_folder, recursive = TRUE)
#     message(paste("Created directory:", gfc_folder))
#   }
#
#   if (gfc_dataset_year == "latest") {
#     gfc_dataset_year <- 2023
#   }
#
#   if (!gfc_dataset_year %in% seq(2023, 2015)) {
#     stop("Invalid gfc_dataset_year. Please use a year between 2015 and 2023.")
#   }
#
#   dataset_str <- paste0("GFC-", gfc_dataset_year, "-v1.", (gfc_dataset_year-2018) + 6)
#
#   # Use custom forest mask if provided
#   if (!is.null(forest_mask)) {
#     tryCatch({
#       if (weighted_mean) {
#         # Extract values with weights
#         extracted_vals <- terra::extract(forest_mask, sf::st_as_sf(roi), weights = TRUE, normalizeWeights = FALSE)
#         if (!is.null(extracted_vals) && nrow(extracted_vals) > 0) {
#           # Convert to binary forest cover (1 = forest, 0 = non-forest)
#           extracted_vals[, 2] <- ifelse(extracted_vals[, 2] != 1, 0, 1)
#           # Calculate weighted mean for each threshold
#           for (threshold in thresholds) {
#             tmp <- extracted_vals
#             tmp[, 2] <- ifelse(tmp[, 2] > threshold, 1.0, 0.0)
#             if (sum(tmp[, 3]) > 0) {
#               forest_cover <- c(forest_cover, sum(tmp[, 2] * tmp[, 3]) / sum(tmp[, 3]))
#             }
#           }
#         }
#       } else {
#         # Unweighted case
#         extracted_vals <- terra::extract(forest_mask, sf::st_as_sf(roi))[[1]]
#         if (!is.null(extracted_vals)) {
#           # Convert to binary forest cover (1 = forest, 0 = non-forest)
#           extracted_vals <- ifelse(extracted_vals != 1, 0, 1)
#           # Calculate mean for each threshold
#           for (threshold in thresholds) {
#             tmp <- ifelse(extracted_vals > threshold, 1.0, 0.0)
#             forest_cover <- c(forest_cover, mean(tmp, na.rm = TRUE))
#           }
#         }
#       }
#     }, error = function(e) {
#       message("Error extracting values from forest_mask: ", e$message)
#     })
#   } else {
#     # Get Hansen GFC tree cover tile names for the ROI
#     #ras <- TCtileNames(roi, gfc_dataset_year)
#     gfcTile <- suppressMessages(suppressWarnings(gfcanalysis::calc_gfc_tiles(roi)))
#     gfcanalysis::download_tiles(gfcTile, gfc_folder, images = "treecover2000", dataset = dataset_str)
#
#     # Get overlapping tile/s (up to 4 possible tiles)
#     bb <- sf::st_bbox(roi)
#     crds <- expand.grid(x = bb[c(1, 3)], y = bb[c(2, 4)])
#     fnms <- character(4)
#
#     for (i in 1:nrow(crds)) {
#       lon <- 10 * (crds[i, 1] %/% 10)
#       lat <- 10 * (crds[i, 2] %/% 10) + 10
#       LtX <- ifelse(lon < 0, "W", "E")
#       LtY <- ifelse(lat < 0, "S", "N")
#       WE <- paste0(sprintf('%03d', abs(lon)), LtX)
#       NS <- paste0(sprintf('%02d', abs(lat)), LtY)
#
#       fnms[i] <- paste0("Hansen_", dataset_str, "_treecover2000_", NS, "_", WE, ".tif")
#     }
#
#     # Process each tile
#     forest_cover_all <- lapply(unique(fnms), function(f) {
#       # Print the current file being processed
#       message("Processing tile: ", f)
#
#       # Verify if the file was downloaded successfully
#       if (!file.exists(file.path(gfc_folder, f))) {
#         stop(paste0("Failed to download ", f, ". Please try again later or check the input arguments."))
#       }
#
#       # Try to extract values from the raster
#       tryCatch({
#         raster_obj <- terra::rast(file.path(gfc_folder, f))
#
#         message("Extracting values for ROI...")
#         extracted_vals <- terra::extract(raster_obj, sf::st_as_sf(roi), weights = weighted_mean, normalizeWeights = FALSE)
#         colnames(extracted_vals)[2] <- "treecover"
#
#         message("Extraction complete")
#       }, error = function(e) {
#         message("Error extracting values from ", f, ": ", e$message)
#         return(NULL)
#       })
#
#       # Return extracted values
#       return(extracted_vals)
#     })
#
#     # Combine all extracted values
#     combined_vals <- dplyr::bind_rows(forest_cover_all)
#
#     # Calculate forest cover for each threshold
#     if (!is.null(combined_vals) && nrow(combined_vals) > 0) {
#       for (threshold in thresholds) {
#         if (weighted_mean) {
#           # Weighted mean case
#           tmp <- combined_vals
#           tmp$treecover <- ifelse(tmp$treecover > threshold, 1.0, 0.0)
#           if (sum(tmp$weight) > 0) {
#             forest_cover <- c(forest_cover, sum(tmp$treecover * tmp$weight) / sum(tmp$weight))
#           }
#         } else {
#           # Unweighted mean case
#           tmp <- combined_vals$treecover
#           tmp <- ifelse(tmp > threshold, 1.0, 0.0)
#           forest_cover <- c(forest_cover, mean(tmp, na.rm = TRUE))
#         }
#       }
#     }
#   }
#
#   return(forest_cover)
# }









# # Function to sample block mean mapped AGB over a polygon
# old_sampleAGBmap <- function(pol, wghts = FALSE, own = TRUE, AGBown) {
#   AGB <- NA
#
#   if (own == TRUE) {
#     AGBown[AGBown == 0] <- NA
#     vls <- matrix(ncol = 2, nrow = 0)
#
#     tryCatch({
#       extracted_vals <- extract(AGBown, st_as_sf(pol), weights = TRUE, normalizeWeights = FALSE)
#       if (!is.null(extracted_vals) && length(extracted_vals[[2]]) > 0) {
#         vls <- rbind(vls, extracted_vals[[2]])
#       }
#     }, error = function(e) {
#       message("Error extracting values from AGBown: ", e$message)
#     })
#
#     if (nrow(vls) > 0) {
#       ids <- which(!is.na(vls[,1]))
#       vls <- vls[ids,]
#       if (length(vls) == 2) {
#         AGB <- vls[1]
#       } else if (nrow(vls) > 0) {
#         AGB <- sum(apply(vls, 1, prod)) / sum(vls[,2])
#       }
#     }
#   } else {
#     ras <- AGBtileNames(pol)
#
#     if (wghts) {
#       vls <- matrix(ncol = 2, nrow = 0)
#       for (f in ras) {
#         if (file.exists(f)) {
#           tryCatch({
#             raster_obj <- rast(f)
#             extracted_vals <- extract(raster_obj, st_as_sf(pol), weights = TRUE, normalizeWeights = FALSE)
#             if (!is.null(extracted_vals) && length(extracted_vals[[2]]) > 0) {
#               vls <- rbind(vls, extracted_vals[[2]])
#             }
#           }, error = function(e) {
#             message("Error extracting values from ", f, ": ", e$message)
#           })
#         }
#       }
#
#       if (nrow(vls) > 0) {
#         ids <- which(!is.na(vls[,1]))
#         vls <- vls[ids,]
#         if (length(vls) == 2) {
#           AGB <- vls[1]
#         } else if (nrow(vls) > 0) {
#           AGB <- sum(apply(vls, 1, prod)) / sum(vls[,2])
#         }
#       }
#     } else {
#       vls <- numeric()
#       for (f in ras) {
#         if (file.exists(f)) {
#           tryCatch({
#             raster_obj <- rast(f)
#             extracted_vals <- extract(raster_obj, st_as_sf(pol))
#             if (!is.null(extracted_vals) && length(extracted_vals[[2]]) > 0) {
#               vls <- c(vls, extracted_vals[[2]])
#             }
#           }, error = function(e) {
#             message("Error extracting values from ", f, ": ", e$message)
#           })
#         }
#       }
#
#       if (length(na.omit(vls)) > 0) {
#         AGB <- mean(vls, na.rm = TRUE)
#       }
#     }
#   }
#
#   return(AGB[1])
# }
#
# # Function to sample block mean mapped forest over a polygon
# old_sampleTreeCover <- function(pol, thresholds, wghts = FALSE, fmask = NA) {
#   TCs <- numeric()
#
#   if (!inherits(fmask, 'SpatRaster')) {
#     ras <- TCtileNames(pol)
#     pol <- st_as_sf(pol)
#
#     if (wghts) {
#       vls <- matrix(ncol = 2, nrow = 0)
#       for (f in ras) {
#         if (file.exists(f)) {
#           tryCatch({
#             raster_obj <- rast(f)
#             extracted_vals <- extract(raster_obj, pol, weights = TRUE, normalizeWeights = FALSE)
#             if (!is.null(extracted_vals) && length(extracted_vals[[2]]) > 0) {
#               vls <- rbind(vls, extracted_vals[[2]])
#             }
#           }, error = function(e) {
#             message("Error extracting values from ", f, ": ", e$message)
#           })
#         }
#       }
#
#       if (nrow(vls) > 0) {
#         ids <- which(!is.na(vls[,1]))
#         vls <- vls[ids,]
#         if (length(vls) == 2) vls <- matrix(vls, ncol = 2)
#
#         for (threshold in thresholds) {
#           tmp <- vls
#           tmp[,1] <- ifelse(tmp[,1] > threshold, 1.0, 0.0)
#           if (sum(tmp[,2]) > 0) {
#             TCs <- c(TCs, sum(apply(tmp, 1, prod)) / sum(tmp[,2]))
#           }
#         }
#       }
#     } else {
#       vls <- numeric()
#       for (f in ras) {
#         if (file.exists(f)) {
#           tryCatch({
#             raster_obj <- rast(f)
#             extracted_vals <- extract(raster_obj, pol)
#             if (!is.null(extracted_vals) && length(extracted_vals[[2]]) > 0) {
#               vls <- c(vls, extracted_vals[[2]])
#             }
#           }, error = function(e) {
#             message("Error extracting values from ", f, ": ", e$message)
#           })
#         }
#       }
#
#       if (length(vls) > 0) {
#         for (threshold in thresholds) {
#           TCs <- c(TCs, mean(ifelse(vls > threshold, 1.0, 0.0), na.rm = TRUE))
#         }
#       }
#     }
#   } else {
#     vls <- extract(fmask, pol)[[1]]
#     if (!is.null(vls)) {
#       vls <- ifelse(vls != 1, 0, 1)
#       TCs <- mean(vls)
#     }
#   }
#
#   return(TCs[1])
# }

