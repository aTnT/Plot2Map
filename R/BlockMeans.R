## Updates made to the new framework:
# 16/01/2025:
# Used sf and terra packages consistently for spatial operations.
# Improved error handling with tryCatch.
# Ensured compatibility with both sf and terra input types.
# Removed dependencies on non-CRAN packages.
# 01/02/2025:
# Automatic donwload of ESA CCI Biomass tile data if not available locally
# Increased 25x the speed in the weighted mean case calculation (with no parallelism yet)

## Notes:
# `AGBown` and `own` arguments are redundant, if there's no problem with not breaking old legacy code, `own` argument shall be removed.
# sampleAGBmap was re-written because it was broken. For this reason we cannot fully compare old vs new functions, so we need a reproducible example with expected output result to validate old vs new functions.
# Do we only calculate mean AGB here, no need for SD for uncertainty propagation?





#' Sample block mean mapped AGB over a region of interest
#'
#' This function samples the block mean mapped Above Ground Biomass (AGB) over a given polygon.
#' It can use either a custom AGB map provided as input or download and use ESA CCI BIOMASS AGB tiles.
#'
#' @param roi An sf or SpatVector object representing the Region of Interest.
#' @param weighted_mean Logical, if TRUE the weighted mean is calculated considering the
#'  approximate fraction of each cell that is covered by the polygon (default is FALSE).
#' @param agb_raster A SpatRaster object with the custom AGB map. If NULL, ESA CCI BIOMASS AGB tiles will be downloaded and used.
#' This dataset comprises estimates of forest above-ground biomass for the years 2010, 2015, 2016, 2017, 2018, 2019,
#' 2020 and 2021. They are derived from a combination of Earth observation data, depending on the year, from the Copernicus
#' Sentinel-1 mission, Envisat’s ASAR (Advanced Synthetic Aperture Radar) instrument and JAXA’s (Japan Aerospace Exploration
#' Agency) Advanced Land Observing Satellite (ALOS-1 and ALOS-2), along with additional information from Earth observation
#' sources. The data has been produced as part of the European Space Agency's (ESA's) Climate Change Initiative (CCI)
#' programme by the Biomass CCI team.
#' @inheritParams download_esacci_biomass
#'
#' @return Numeric value representing the mean AGB for the polygon.
#'
#' @import parallel pblapply
#' @importFrom sf st_as_sf
#' @importFrom terra rast extract
#'
#' @export
#'
#' @references [Santoro, M.; Cartus, O. (2024): ESA Biomass Climate Change Initiative (Biomass_cci): Global datasets of forest above-ground biomass for the years 2010, 2015, 2016, 2017, 2018, 2019, 2020 and 2021, v5.01. NERC EDS Centre for Environmental Data Analysis, 22 August 2024.](https://dx.doi.org/10.5285/bf535053562141c6bb7ad831f5998d77)
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
#' # Example 1: Calculate mean AGB for the Congo ROI (unweighted)
#' sampleAGBmap(roi_sf_congo)
#'
#' # Example 2: Calculate mean AGB for the Congo ROI (weighted)
#' sampleAGBmap(roi_sf_congo, weighted_mean = TRUE)
#'
sampleAGBmap <- function(
    roi,
    weighted_mean = FALSE,
    agb_raster = NULL,
    esacci_biomass_year = "latest",
    esacci_biomass_version = "latest",
    output_dir = "data/ESACCI-BIOMASS",
    n_cores = 1,
    timeout = 600
) {

  #AGB <- NA

  if (!is.null(agb_raster)) {
    agb_raster[agb_raster == 0] <- NA
    #vls <- matrix(ncol = 2, nrow = 0)


    # TO DO: this case assumes weighted_mean = TRUE

    tryCatch({
      extracted_vals <- terra::extract(agb_raster, sf::st_as_sf(roi), weights = TRUE, normalizeWeights = FALSE)
      if (!is.null(extracted_vals) && length(extracted_vals[[2]]) > 0) {
        #vls <- rbind(vls, extracted_vals[[2]])
      }
    }, error = function(e) {
      message("Error extracting values from agb_raster: ", e$message)
    })

    #if (nrow(vls) > 0) {
    if (nrow(extracted_vals) > 0) {
      ids <- which(!is.na(extracted_vals[,2]))
      vls <- extracted_vals[ids,]
      if (length(vls) == 2) {
        AGB <- mean(vls[,2], na.rm = TRUE)
      } else if (length(vls) > 2) {
        AGB <- sum(apply(vls[,2:3], 1, prod)) / sum(vls[,3])
      }
    }
  } else {

    # Get ESA CCI AGB raster data:
    esacci_biomass_args <- validate_esacci_biomass_args (esacci_biomass_year, esacci_biomass_version)

    #ras <- AGBtileNames(roi)
    ras <- ESACCIAGBtileNames(roi, esacci_biomass_args$esacci_biomass_year, esacci_biomass_args$esacci_biomass_version)

    AGB_all <- lapply(ras, function(f) {
      # Print the current file being processed
      message("Processing file: ", f)

      # Check if the file already exists
      if (!file.exists(file.path(output_dir, f))) {
        message("File not found locally. Attempting to download...")

        # Download the file
        download_esacci_biomass(
          esacci_biomass_year = esacci_biomass_year,
          esacci_biomass_version = esacci_biomass_version,
          output_dir = output_dir,
          n_cores = n_cores,
          timeout = timeout,
          file_names = f
        )

        # Verify if the file was downloaded successfully
        if (!file.exists(file.path(output_dir, f))) {
          stop(paste0("Something went wrong with downloading ", f, ". Please try again later or check the input arguments."))
        } else {
          message("Download successful: ", f)
        }
      } else {
        message("File already exists locally")
      }

      # Try to extract values from the raster
      tryCatch({
        message("Loading raster")
        raster_obj <- terra::rast(file.path(output_dir, f))

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

  }

  return(AGB)
}




#' Sample block mean mapped forest over a polygon
#'
#' This function samples the block mean mapped forest cover over a given polygon.
#' It can use either predefined tree cover tiles or a custom forest mask.
#'
#' @inheritParams sampleAGBmap
#' @param thresholds Numeric vector of tree cover thresholds to apply.
#' @param fmask A SpatRaster object representing a custom forest mask (default is NA).
#'
#' @return Numeric vector of tree cover percentages for each threshold.
#'
#' @importFrom sf st_as_sf
#' @importFrom terra rast extract
#'
#' @export
sampleTreeCover <- function(roi, thresholds, weighted_mean = FALSE, fmask = NA) {
  TCs <- numeric()

  if (!inherits(fmask, 'SpatRaster')) {
    ras <- TCtileNames(roi)
    roi <- sf::st_as_sf(roi)

    if (weighted_mean) {
      vls <- matrix(ncol = 2, nrow = 0)
      for (f in ras) {
        if (file.exists(f)) {
          tryCatch({
            raster_obj <- terra::rast(f)
            extracted_vals <- terra::extract(raster_obj, roi, weights = TRUE, normalizeWeights = FALSE)
            if (!is.null(extracted_vals) && length(extracted_vals[[2]]) > 0) {
              vls <- rbind(vls, extracted_vals[[2]])
            }
          }, error = function(e) {
            message("Error extracting values from ", f, ": ", e$message)
          })
        }
      }

      if (nrow(vls) > 0) {
        ids <- which(!is.na(vls[,1]))
        vls <- vls[ids,]
        if (length(vls) == 2) vls <- matrix(vls, ncol = 2)

        for (threshold in thresholds) {
          tmp <- vls
          tmp[,1] <- ifelse(tmp[,1] > threshold, 1.0, 0.0)
          if (sum(tmp[,2]) > 0) {
            TCs <- c(TCs, sum(apply(tmp, 1, prod)) / sum(tmp[,2]))
          }
        }
      }
    } else {
      vls <- numeric()
      for (f in ras) {
        if (file.exists(f)) {
          tryCatch({
            raster_obj <- terra::rast(f)
            extracted_vals <- terra::extract(raster_obj, roi)
            if (!is.null(extracted_vals) && length(extracted_vals[[2]]) > 0) {
              vls <- c(vls, extracted_vals[[2]])
            }
          }, error = function(e) {
            message("Error extracting values from ", f, ": ", e$message)
          })
        }
      }

      if (length(vls) > 0) {
        for (threshold in thresholds) {
          TCs <- c(TCs, mean(ifelse(vls > threshold, 1.0, 0.0), na.rm = TRUE))
        }
      }
    }
  } else {
    vls <- terra::extract(fmask, roi)[[1]]
    if (!is.null(vls)) {
      vls <- ifelse(vls != 1, 0, 1)
      TCs <- mean(vls)
    }
  }

  return(TCs[1])
}






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
#
# # Tests
#
# library(testthat)
# library(sf)
# library(terra)
#
# # Test comparison between old and new versions
# test_that("Old and new sampleAGBmap functions produce consistent results", {
#
#   skip("APIs now very different between old and new framework")
#   # Create a sample polygon
#   # Amazon
#   # roi <- st_polygon(list(rbind(c(-62.2159, -3.4653), c(-62.2059, -3.4653),
#   #                              c(-62.2059, -3.4553), c(-62.2159, -3.4553),
#   #                              c(-62.2159, -3.4653))))
#   # roi_sf <- st_sfc(roi, crs = 4326)
#
#   # Mexico
#   roi <- st_polygon(list(rbind(c(-100.5,18), c(-101,19), c(-101,19), c(-100.5,19), c(-100.5,18))))
#   roi_sf <- st_sfc(roi, crs = 4326)
#
#   # Mock agb_raster
#   set.seed(42)
#   #agb_raster <- rast(nrows=10, ncols=10, xmin=-63, xmax=-62, ymin=-4, ymax=-3, vals=runif(100, 0, 500))
#   agb_raster <- rast(nrows=10, ncols=10, xmin=-101, xmax=-100, ymin=18, ymax=19, vals=runif(100, 0, 500))
#
#   # Test with own=TRUE
#   old_result_own <- old_sampleAGBmap(roi_sf, wghts=FALSE, own=TRUE, AGBown=agb_raster)
#   new_result_own <- sampleAGBmap(roi_sf, weighted_mean=FALSE, own=TRUE, agb_raster=agb_raster)
#   expect_equal(old_result_own, new_result_own, tolerance=1e-6)
#   expect_equal(new_result_own, 457.403, tolerance=1e-6)
#
#
#   # Real-world agb_raster (same AGB as with own=FALSE)
#   agb_raster <- rast(AGBtileNames(roi_sf))
#
#   # Test with own=TRUE, real-world
#   old_result_own <- old_sampleAGBmap(roi_sf, wghts=FALSE, own=TRUE, AGBown=agb_raster)
#   new_result_own <- sampleAGBmap(roi_sf, weighted_mean=FALSE, own=TRUE, agb_raster=agb_raster)
#   expect_equal(old_result_own, new_result_own, tolerance=1e-6)
#   expect_equal(new_result_own, 89, tolerance=1e-6)
#
#
#
#   # Test with own=FALSE, 1 tile
#   old_result_tiles <- old_sampleAGBmap(roi_sf, wghts=FALSE, own=FALSE)
#   new_result_tiles <- sampleAGBmap(roi_sf, weighted_mean=FALSE, own=FALSE)
#   expect_equal(old_result_tiles, new_result_tiles, tolerance=1e-6)
#   expect_equal(new_result_tiles, 33.29543, tolerance=1e-6)
#   expect_equal(new_result_own, new_result_tiles, tolerance=1e-6)
#
#   # Test with own=FALSE, 2 tiles
#   # Mexico
#   roi2 <- st_polygon(list(rbind(c(-99,18), c(-101,19), c(-101,19), c(-99,19), c(-99,18))))
#   roi2_sf <- st_sfc(roi2, crs = 4326)
#
#   roi2_tiles <- AGBtileNames(roi2_sf)
#   expect_equal(length(roi2_tiles), 2)
#
#   old_result_roi2_tiles <- old_sampleAGBmap(roi2_sf, wghts=FALSE, own=FALSE)
#   new_result_roi2_tiles <- sampleAGBmap(roi2_sf, weighted_mean=FALSE, own=FALSE)
#   expect_equal(old_result_roi2_tiles, new_result_roi2_tiles, tolerance=1e-6)
#
#
#
# })

# test_that("Old and new sampleTreeCover functions produce consistent results", {
#   #roi <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
#   #roi_sf <- st_sfc(roi, crs = 4326)
#   roi <- st_polygon(list(rbind(c(687300,8907200), c(688300,8907200), c(688300,8908200), c(687300,8908200), c(687300,8907200))))
#   roi_sf <- st_sfc(roi, crs = 32719)
#
#   thresholds <- 10
#
#   # Forest mask raster
#   #fmask <- rast(nrows=10, ncols=10, xmin=0, xmax=1, ymin=0, ymax=1, vals=sample(0:1, 100, replace=TRUE))
#   fmask_raster <- raster::raster('data/SustainableLandscapeBrazil_v04/SLB_CVmaps/BON_A01_2018_CV_100m.tif')
#   fmask_terra <- terra::rast('data/SustainableLandscapeBrazil_v04/SLB_CVmaps/BON_A01_2018_CV_100m.tif')
#
#
#
#
#
#   # roi_sf <- st_read("data/maap_plot2map_data/Wales_boundary.shp")
#   # # Convert MULTIPOLYGON to POLYGON
#   # roi_sf <- st_cast(roi_sf, "POLYGON")
#
#   roi <- st_polygon(list(rbind(c(-4,52), c(-3,52), c(-3,53), c(-4,53), c(-4,52))))
#   roi_sf <- st_sfc(roi, crs = 4326)
#
#
#   thresholds <- 10
#
#   # Forest mask raster
#   fmask_raster <- raster::raster('data/maap_plot2map_data/WoodyMask_Wales_2020.tif')
#   fmask_terra <- terra::rast('data/maap_plot2map_data/WoodyMask_Wales_2020.tif')
#
#
#   old_result <- old_sampleTreeCover(roi_sf, thresholds, wghts=FALSE, fmask=NA)
#   new_result <- sampleTreeCover(roi_sf, thresholds, weighted_mean=FALSE, fmask=NA)
#   expect_equal(old_result, new_result, tolerance=1e-6)
#
#   old_result_mask <- old_sampleTreeCover(roi_sf, thresholds, wghts=FALSE, fmask=fmask_raster)
#   new_result_mask <- sampleTreeCover(roi_sf, thresholds, weighted_mean=FALSE, fmask=fmask_raster)
#   expect_equal(old_result_mask, new_result_mask, tolerance=1e-6)
#
#   old_result_mask_terra <- old_sampleTreeCover(roi_sf, thresholds, wghts=FALSE, fmask=fmask_terra)
#   new_result_mask_terra <- sampleTreeCover(roi_sf, thresholds, weighted_mean=FALSE, fmask=fmask_terra)
#   expect_equal(old_result_mask_terra, new_result_mask_terra, tolerance=1e-6)
#
# })
#
# # Test internal consistency
# test_that("sample#' @import stringrAGBmap function behaves consistently", {
#   roi <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
#   roi_sf <- st_sfc(roi, crs = 4326)
#
#   AGBown <- rast(nrows=10, ncols=10, xmin=0, xmax=1, ymin=0, ymax=1, vals=runif(100, 0, 500))
#
#   result_own <- new_sampleAGBmap(roi_sf, wghts=FALSE, own=TRUE)
#   expect_type(result_own, "double")
#   expect_true(!is.na(result_own))
#
#   result_tiles <- new_sampleAGBmap(roi_sf, wghts=FALSE, own=FALSE)
#   expect_type(result_tiles, "double")
# })
#
# test_that("sampleTreeCover function behaves consistently", {
#   roi <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
#   roi_sf <- st_sfc(roi, crs = 4326)
#
#   thresholds <- c(10, 30, 50)
#   fmask <- rast(nrows=10, ncols=10, xmin=0, xmax=1, ymin=0, ymax=1, vals=sample(0:1, 100, replace=TRUE))
#
#   result <- new_sampleTreeCover(roi_sf, thresholds, wghts=FALSE, fmask=NA)
#   expect_type(result, "double")
#   expect_length(result, length(thresholds))
#
#   result_mask <- new_sampleTreeCover(roi_sf, thresholds, wghts=FALSE, fmask=fmask)
#   expect_type(result_mask, "double")
#   expect_length(result_mask, 1)
# })


# test_that("sampleAGBmap function behaves consistently - 1 tile cases", {
#   # Amazon Rainforest
#   roi_amazon <- st_polygon(list(rbind(c(-62.2159, -3.4653), c(-62.2059, -3.4653),
#                                       c(-62.2059, -3.4553), c(-62.2159, -3.4553),
#                                       c(-62.2159, -3.4653))))
#   roi_sf_amazon <- st_sfc(roi_amazon, crs = 4326)
#
#   # Congo Rainforest
#   roi_congo <- st_polygon(list(rbind(c(25.0089, 0.4735), c(25.0189, 0.4735),
#                                      c(25.0189, 0.4835), c(25.0089, 0.4835),
#                                      c(25.0089, 0.4735))))
#   roi_sf_congo <- st_sfc(roi_congo, crs = 4326)
#
#   # Test with both polygons
#   result_amazon <- sampleAGBmap(roi_sf_amazon)
#   result_amazon_weighted_mean <- sampleAGBmap(roi_sf_amazon, weighted_mean = TRUE)
#   result_congo <- sampleAGBmap(roi_sf_congo)
#   result_congo_weighted_mean <- sampleAGBmap(roi_sf_congo, weighted_mean = TRUE)
#
#   expect_type(result_amazon, "double")
#   expect_type(result_congo, "double")
#   expect_type(result_amazon_weighted_mean, "double")
#   expect_type(result_congo_weighted_mean, "double")
#   expect_true(result_amazon != result_amazon_weighted_mean)
#   expect_true(result_congo != result_congo_weighted_mean)
# })
#
# test_that("sampleAGBmap function behaves consistently - several tiles case", {
#   # Amazon Rainforest
#   roi_amazon <- st_polygon(list(rbind(c(-62.2159, -3.4653), c(-62.2059, -3.4653),
#                                       c(-62.2059, -3.4553), c(-62.2159, -3.4553),
#                                       c(-62.2159, -3.4653))))
#   roi_sf_amazon <- st_sfc(roi_amazon, crs = 4326)
#   roi_sf_amazon <- st_buffer(roi_sf_amazon, dist = 4)
#
#
#   # Test with both polygons
#   result_amazon <- sampleAGBmap(roi_sf_amazon)
#
#   expect_type(result_amazon, "double")
# })





# test_that("sampleTreeCover function behaves consistently", {
#   # Daintree Rainforest
#   roi_daintree <- st_polygon(list(rbind(c(145.3833, -16.2500), c(145.3933, -16.2500),
#                                         c(145.3933, -16.2400), c(145.3833, -16.2400),
#                                         c(145.3833, -16.2500))))
#   roi_sf_daintree <- st_sfc(roi_daintree, crs = 4326)
#
#   # Sumatra Rainforest
#   roi_sumatra <- st_polygon(list(rbind(c(101.3431, -0.5897), c(101.3531, -0.5897),
#                                        c(101.3531, -0.5797), c(101.3431, -0.5797),
#                                        c(101.3431, -0.5897))))
#   roi_sf_sumatra <- st_sfc(roi_sumatra, crs = 4326)
#
#   thresholds <- c(10, 30, 50)
#
#   result_daintree <- sampleTreeCover(roi_sf_daintree, thresholds, wghts=FALSE, fmask=NA)
#   result_sumatra <- sampleTreeCover(roi_sf_sumatra, thresholds, wghts=FALSE, fmask=NA)
#
#   expect_type(result_daintree, "double")
#   expect_type(result_sumatra, "double")
#   expect_length(result_daintree, length(thresholds))
#   expect_length(result_sumatra, length(thresholds))
# })


