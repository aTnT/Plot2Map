## Updates made to the new framework:
# 16/01/2025:
# Used sf and terra packages consistently for spatial operations.
# Improved error handling with tryCatch.
# Ensured compatibility with both sf and terra input types.
# Removed dependencies on non-CRAN packages.

## Notes:
# `AGBown` and `own` arguments are redundant, if there's no problem with not breaking old legacy code, `own` argument shall be removed.



#' Sample block mean mapped AGB over a polygon
#'
#' This function samples the block mean mapped Above Ground Biomass (AGB) over a given polygon.
#' It can use either a custom AGB map or predefined AGB tiles.
#'
#' @param pol An sf or SpatVector object representing the polygon of interest.
#' @param wghts Logical, if TRUE the weighted mean is calculated considering the
#'  approximate fraction of each cell that is covered by the polygon (default is FALSE).
#' @param own Logical, whether to use a custom AGB map (default is TRUE).
#' @param AGBown A SpatRaster object with the custom AGB map.
#'
#' @return Numeric value representing the mean AGB for the polygon.
#'
#' @importFrom sf st_as_sf
#' @importFrom terra rast extract
#'
#' @export
sampleAGBmap <- function(pol, wghts = FALSE, own = TRUE, AGBown) {
  AGB <- NA

  if (own) {
    AGBown[AGBown == 0] <- NA
    #vls <- matrix(ncol = 2, nrow = 0)

    tryCatch({
      extracted_vals <- terra::extract(AGBown, sf::st_as_sf(pol), weights = TRUE, normalizeWeights = FALSE)
      if (!is.null(extracted_vals) && length(extracted_vals[[2]]) > 0) {
        #vls <- rbind(vls, extracted_vals[[2]])
      }
    }, error = function(e) {
      message("Error extracting values from AGBown: ", e$message)
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
    ras <- AGBtileNames(pol)

    if (wghts) {
      # vls <- matrix(ncol = 2, nrow = 0)
      for (f in ras) {
        if (file.exists(f)) {
          tryCatch({
            raster_obj <- terra::rast(f)
            extracted_vals <- terra::extract(raster_obj, sf::st_as_sf(pol), weights = TRUE, normalizeWeights = FALSE)
            if (!is.null(extracted_vals) && length(extracted_vals[[2]]) > 0) {
              #vls <- rbind(vls, extracted_vals[[2]])
            }
          }, error = function(e) {
            message("Error extracting values from ", f, ": ", e$message)
          })
        }
      }

      if (nrow(vls) > 0) {
        ids <- which(!is.na(extracted_vals[,2]))
        vls <- extracted_vals[ids,]
        if (length(vls) == 2) {  # do we need this case? don't know how this could happen (lenght of 2)
          AGB <- mean(vls[,2], na.rm = TRUE)
        } else if (length(vls) > 2) {
          AGB <- sum(apply(vls[,2:3], 1, prod)) / sum(vls[,3])
        }
      }
    } else {
      #vls <- numeric()
      for (f in ras) {
        if (file.exists(f)) {
          tryCatch({
            raster_obj <- terra::rast(f)
            extracted_vals <- terra::extract(raster_obj, sf::st_as_sf(pol))
            if (!is.null(extracted_vals) && length(extracted_vals[[2]]) > 0) {
              vls <- extracted_vals[[2]]
            }
          }, error = function(e) {
            message("Error extracting values from ", f, ": ", e$message)
          })
        }
      }

      if (length(na.omit(vls)) > 0) {
        AGB <- mean(vls, na.rm = TRUE)
      }
    }
  }

  return(AGB)
}




#' Sample block mean mapped forest over a polygon
#'
#' This function samples the block mean mapped forest cover over a given polygon.
#' It can use either predefined tree cover tiles or a custom forest mask.
#'
#' @param pol An sf or SpatVector object representing the polygon of interest.
#' @param thresholds Numeric vector of tree cover thresholds to apply.
#' @param wghts Logical, whether to use weighted extraction (default is FALSE).
#' @param fmask A SpatRaster object representing a custom forest mask (default is NA).
#'
#' @return Numeric vector of tree cover percentages for each threshold.
#'
#' @importFrom sf st_as_sf
#' @importFrom terra rast extract
#'
#' @export
sampleTreeCover <- function(pol, thresholds, wghts = FALSE, fmask = NA) {
  TCs <- numeric()

  if (!inherits(fmask, 'SpatRaster')) {
    ras <- TCtileNames(pol)
    pol <- sf::st_as_sf(pol)

    if (wghts) {
      vls <- matrix(ncol = 2, nrow = 0)
      for (f in ras) {
        if (file.exists(f)) {
          tryCatch({
            raster_obj <- terra::rast(f)
            extracted_vals <- terra::extract(raster_obj, pol, weights = TRUE, normalizeWeights = FALSE)
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
            extracted_vals <- terra::extract(raster_obj, pol)
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
    vls <- terra::extract(fmask, pol)[[1]]
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

# Tests

# library(testthat)
# library(sf)
# library(terra)
#
# # Test comparison between old and new versions
# test_that("Old and new sampleAGBmap functions produce consistent results", {
#   # Create a sample polygon
#   # Amazon
#   # pol <- st_polygon(list(rbind(c(-62.2159, -3.4653), c(-62.2059, -3.4653),
#   #                              c(-62.2059, -3.4553), c(-62.2159, -3.4553),
#   #                              c(-62.2159, -3.4653))))
#   # pol_sf <- st_sfc(pol, crs = 4326)
#
#   # Mexico
#   pol <- st_polygon(list(rbind(c(-100.5,18), c(-101,19), c(-101,19), c(-100.5,19), c(-100.5,18))))
#   pol_sf <- st_sfc(pol, crs = 4326)
#
#   # Mock AGBown raster
#   set.seed(42)
#   #AGBown <- rast(nrows=10, ncols=10, xmin=-63, xmax=-62, ymin=-4, ymax=-3, vals=runif(100, 0, 500))
#   AGBown <- rast(nrows=10, ncols=10, xmin=-101, xmax=-100, ymin=18, ymax=19, vals=runif(100, 0, 500))
#
#   # Test with own=TRUE
#   old_result_own <- old_sampleAGBmap(pol_sf, wghts=FALSE, own=TRUE, AGBown=AGBown)
#   new_result_own <- sampleAGBmap(pol_sf, wghts=FALSE, own=TRUE, AGBown=AGBown)
#   expect_equal(old_result_own, new_result_own, tolerance=1e-6)
#   expect_equal(new_result_own, 457.403, tolerance=1e-6)
#
#
#   # Real-world AGBown (same AGB as with own=FALSE)
#   AGBown <- rast(AGBtileNames(pol_sf))
#
#   # Test with own=TRUE, real-world
#   old_result_own <- old_sampleAGBmap(pol_sf, wghts=FALSE, own=TRUE, AGBown=AGBown)
#   new_result_own <- sampleAGBmap(pol_sf, wghts=FALSE, own=TRUE, AGBown=AGBown)
#   expect_equal(old_result_own, new_result_own, tolerance=1e-6)
#   expect_equal(new_result_own, 89, tolerance=1e-6)
#
#
#
#   # Test with own=FALSE, 1 tile
#   old_result_tiles <- old_sampleAGBmap(pol_sf, wghts=FALSE, own=FALSE)
#   new_result_tiles <- sampleAGBmap(pol_sf, wghts=FALSE, own=FALSE)
#   expect_equal(old_result_tiles, new_result_tiles, tolerance=1e-6)
#   expect_equal(new_result_tiles, 33.29543, tolerance=1e-6)
#   expect_equal(new_result_own, new_result_tiles, tolerance=1e-6)
#
#   # Test with own=FALSE, 2 tiles
#   # Mexico
#   pol2 <- st_polygon(list(rbind(c(-99,18), c(-101,19), c(-101,19), c(-99,19), c(-99,18))))
#   pol2_sf <- st_sfc(pol2, crs = 4326)
#
#   pol2_tiles <- AGBtileNames(pol2_sf)
#   expect_equal(length(pol2_tiles), 2)
#
#   old_result_pol2_tiles <- old_sampleAGBmap(pol2_sf, wghts=FALSE, own=FALSE)
#   new_result_pol2_tiles <- sampleAGBmap(pol2_sf, wghts=FALSE, own=FALSE)
#   expect_equal(old_result_pol2_tiles, new_result_pol2_tiles, tolerance=1e-6)
#
#
#
# })
#
# test_that("Old and new sampleTreeCover functions produce consistent results", {
#   #pol <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
#   #pol_sf <- st_sfc(pol, crs = 4326)
#   pol <- st_polygon(list(rbind(c(687300,8907200), c(688300,8907200), c(688300,8908200), c(687300,8908200), c(687300,8907200))))
#   pol_sf <- st_sfc(pol, crs = 32719)
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
#   # pol_sf <- st_read("data/maap_plot2map_data/Wales_boundary.shp")
#   # # Convert MULTIPOLYGON to POLYGON
#   # pol_sf <- st_cast(pol_sf, "POLYGON")
#
#   pol <- st_polygon(list(rbind(c(-4,52), c(-3,52), c(-3,53), c(-4,53), c(-4,52))))
#   pol_sf <- st_sfc(pol, crs = 4326)
#
#
#   thresholds <- 10
#
#   # Forest mask raster
#   fmask_raster <- raster::raster('data/maap_plot2map_data/WoodyMask_Wales_2020.tif')
#   fmask_terra <- terra::rast('data/maap_plot2map_data/WoodyMask_Wales_2020.tif')
#
#
#   old_result <- old_sampleTreeCover(pol_sf, thresholds, wghts=FALSE, fmask=NA)
#   new_result <- sampleTreeCover(pol_sf, thresholds, wghts=FALSE, fmask=NA)
#   expect_equal(old_result, new_result, tolerance=1e-6)
#
#   old_result_mask <- old_sampleTreeCover(pol_sf, thresholds, wghts=FALSE, fmask=fmask_raster)
#   new_result_mask <- sampleTreeCover(pol_sf, thresholds, wghts=FALSE, fmask=fmask_raster)
#   expect_equal(old_result_mask, new_result_mask, tolerance=1e-6)
#
#   old_result_mask_terra <- old_sampleTreeCover(pol_sf, thresholds, wghts=FALSE, fmask=fmask_terra)
#   new_result_mask_terra <- sampleTreeCover(pol_sf, thresholds, wghts=FALSE, fmask=fmask_terra)
#   expect_equal(old_result_mask_terra, new_result_mask_terra, tolerance=1e-6)
#
# })
#
# # Test internal consistency
# test_that("sample#' @import stringrAGBmap function behaves consistently", {
#   pol <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
#   pol_sf <- st_sfc(pol, crs = 4326)
#
#   AGBown <- rast(nrows=10, ncols=10, xmin=0, xmax=1, ymin=0, ymax=1, vals=runif(100, 0, 500))
#
#   result_own <- new_sampleAGBmap(pol_sf, wghts=FALSE, own=TRUE)
#   expect_type(result_own, "double")
#   expect_true(!is.na(result_own))
#
#   result_tiles <- new_sampleAGBmap(pol_sf, wghts=FALSE, own=FALSE)
#   expect_type(result_tiles, "double")
# })
#
# test_that("sampleTreeCover function behaves consistently", {
#   pol <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
#   pol_sf <- st_sfc(pol, crs = 4326)
#
#   thresholds <- c(10, 30, 50)
#   fmask <- rast(nrows=10, ncols=10, xmin=0, xmax=1, ymin=0, ymax=1, vals=sample(0:1, 100, replace=TRUE))
#
#   result <- new_sampleTreeCover(pol_sf, thresholds, wghts=FALSE, fmask=NA)
#   expect_type(result, "double")
#   expect_length(result, length(thresholds))
#
#   result_mask <- new_sampleTreeCover(pol_sf, thresholds, wghts=FALSE, fmask=fmask)
#   expect_type(result_mask, "double")
#   expect_length(result_mask, 1)
# })
#
#
# test_that("sampleAGBmap function behaves consistently", {
#   # Amazon Rainforest
#   pol_amazon <- st_polygon(list(rbind(c(-62.2159, -3.4653), c(-62.2059, -3.4653),
#                                       c(-62.2059, -3.4553), c(-62.2159, -3.4553),
#                                       c(-62.2159, -3.4653))))
#   pol_sf_amazon <- st_sfc(pol_amazon, crs = 4326)
#
#   # Congo Rainforest
#   pol_congo <- st_polygon(list(rbind(c(25.0089, 0.4735), c(25.0189, 0.4735),
#                                      c(25.0189, 0.4835), c(25.0089, 0.4835),
#                                      c(25.0089, 0.4735))))
#   pol_sf_congo <- st_sfc(pol_congo, crs = 4326)
#
#   # Test with both polygons
#   result_amazon <- sampleAGBmap(pol_sf_amazon, wghts=FALSE, own=FALSE)
#   result_congo <- sampleAGBmap(pol_sf_congo, wghts=FALSE, own=FALSE)
#
#   expect_type(result_amazon, "double")
#   expect_type(result_congo, "double")
# })
#
# test_that("sampleTreeCover function behaves consistently", {
#   # Daintree Rainforest
#   pol_daintree <- st_polygon(list(rbind(c(145.3833, -16.2500), c(145.3933, -16.2500),
#                                         c(145.3933, -16.2400), c(145.3833, -16.2400),
#                                         c(145.3833, -16.2500))))
#   pol_sf_daintree <- st_sfc(pol_daintree, crs = 4326)
#
#   # Sumatra Rainforest
#   pol_sumatra <- st_polygon(list(rbind(c(101.3431, -0.5897), c(101.3531, -0.5897),
#                                        c(101.3531, -0.5797), c(101.3431, -0.5797),
#                                        c(101.3431, -0.5897))))
#   pol_sf_sumatra <- st_sfc(pol_sumatra, crs = 4326)
#
#   thresholds <- c(10, 30, 50)
#
#   result_daintree <- sampleTreeCover(pol_sf_daintree, thresholds, wghts=FALSE, fmask=NA)
#   result_sumatra <- sampleTreeCover(pol_sf_sumatra, thresholds, wghts=FALSE, fmask=NA)
#
#   expect_type(result_daintree, "double")
#   expect_type(result_sumatra, "double")
#   expect_length(result_daintree, length(thresholds))
#   expect_length(result_sumatra, length(thresholds))
# })


