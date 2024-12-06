## Updates made to the new framework:
# 04/12/2024:
# - Replaced sp functions with sf equivalents.
# - Replaced raster functions with terra equivalents.
# - Updated the function to work with sf objects instead of SpatialPolygonsDataFrame.
# - Removed the global variable SRS and assumed WGS84 as the default coordinate reference system.
# - Possibility to set the Hansen data GFC tiles version, from "GFC-2023-v1.11" (default) to "GFC-2018-v1.6".


## Notes:
# 04/12/2024:
# - Could not test old_Deforested() because of deprecated packages and the related unreproducible environment.
# Instead we use a refactored_old_Deforested() with the minimum changes that make it work and compare old vs new function outputs to
# validate the new function.



#' Remove deforested plots by overlaying plots with Global Forest Change data
#'
#' This function identifies and removes plots that have been deforested based on
#' the Global Forest Change dataset (Hansen et al., 2013). It processes each plot,
#' downloads the necessary forest loss tiles, and determines if the plot has been
#' deforested beyond a 5% threshold or if the deforestation occurred before or during the specified map year.
#'
#' @param plt A data frame or sf object containing plot data. Coordinates are assumed to be in WGS 84 CRS.
#' @param fdir Character string specifying the directory to download GFC forest loss data.
#' @param dataset Numeric value describing which version of the Hansen data to use: 2023 (default) down to 2018.
#' @param map_year Numeric value indicating the year threshold for deforestation (default is 10).
#'
#' @return A list containing two elements:
#'   \item{netPlt}{A data frame of non-deforested plots}
#'   \item{plt}{The original input data frame with an added deforestation indicator}
#'
#' @importFrom sf st_as_sf st_buffer st_bbox st_coordinates
#' @importFrom terra rast extract
#' @importFrom dplyr select setdiff
#' @importFrom gfcanalysis calc_gfc_tiles download_tiles
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   plot_data <- data.frame(POINT_X = c(-3.1, -3.2), POINT_Y = c(51.5, 51.6),
#'                           SIZE_HA = c(1, 1.5), PLOT_ID = c(1, 2))
#'   result <- Deforested(plot_data, "path/to/forest/loss/data", 10)
#' }
Deforested <- function(plt, fdir, dataset = 2023, map_year = 10) {

  # Refactored version, more radical changes (changing to terra, sf ecosystem, etc)

  if (!dataset %in% seq(2023, 2015)) {
    stop("Invalid dataset. Please use a year between 2023 and 2015.")
  }

  dataset_str <- paste0("GFC-", dataset, "-v1.", (dataset-2018) + 6)

  if (!"POINT_X" %in% colnames(plt)) {
    plt$POINT_X <- plt$Xnew
    plt$POINT_Y <- plt$Ynew
    plt$PLOT_ID <- 1:nrow(plt)
  }

  if (!inherits(plt, "sf")) {
    plt <- sf::st_as_sf(plt, coords = c("POINT_X", "POINT_Y"), crs = 4326)
  }

  defo <- numeric(nrow(plt))

  for (p in 1:nrow(plt)) {

    # ww in arc-deg
    ww <- ifelse(!(is.na(plt[p,]$SIZE_HA)),
                 (sqrt(plt[p,]$SIZE_HA*10000) *0.00001)/2, 0.0002) #mean of plots for NAs
    ww <- ifelse(ww < 0, abs(ww), ww)
    sf_use_s2(FALSE)  # ww in sf:: with sf_use_s2(TRUE) is in meters
    pol <- sf::st_buffer(plt[p,], dist = ww, endCapStyle = "SQUARE")
    # diff(st_bbox(pol)[c(2, 4)])
    # diff(st_bbox(pol)[c(1, 3)])

    cat("Processing", round(as.numeric(sf::st_area(pol) / 10000), 2), "ha ... \n")
    #cat(paste('processing:', round((res / 0.00001 * res / 0.00001) / 10000, 2), 'ha')) # checker

    # Downloads respective forest loss tile/s from squared plots
    dir.create(file.path(fdir), showWarnings = FALSE)
    #setwd(file.path(fdir))
    gfcTile <- gfcanalysis::calc_gfc_tiles(pol)
    gfcanalysis::download_tiles(gfcTile, fdir, images = "lossyear", dataset = dataset_str)

    # Get overlapping tile/s (up to 4 possible tiles)
    bb <- sf::st_bbox(pol)
    crds <- expand.grid(x = bb[c(1, 3)], y = bb[c(2, 4)])
    fnms <- character(4)

    for (i in 1:nrow(crds)) {
      lon <- 10 * (crds[i, 1] %/% 10)
      lat <- 10 * (crds[i, 2] %/% 10) + 10
      LtX <- ifelse(lon < 0, "W", "E")
      LtY <- ifelse(lat < 0, "S", "N")
      WE <- paste0(sprintf('%03d', abs(lon)), LtX)
      NS <- paste0(sprintf('%02d', abs(lat)), LtY)

      fnms[i] <- file.path(fdir, paste0("Hansen_", dataset_str, "_lossyear_", NS, "_", WE, ".tif"))
    }

    vls <- numeric()
    for (f in fnms) {
      if (file.exists(f))
        vls <- c(vls, terra::extract(terra::rast(f), pol)[[2]])
    }

    vls <- if (length(vls[vls == 0]) > length(vls[vls > 0])) vls * 0 else (c(vls))
    cat(vls)
    vls[vls > 0] <- 1

    defo[p] <- sum(vls[vls > 0], na.rm = TRUE)
    cat(defo[p])
  }

  plt$defo <- defo
  thresh <- plt$SIZE_HA * 0.05
  cat(paste('Removed', nrow(subset(plt, plt$defo > thresh)), 'plots that have > 5% change'))
  defPlt <- subset(plt, plt$defo > 0)
  defPlt <- subset(defPlt, defPlt$defo <= map_year)
  netPlt <- dplyr::select(dplyr::setdiff(plt, defPlt), -defo)

  return(list(netPlt = netPlt, plt = plt))
}


# old_refactored_Deforested <- function(plt, fdir, map_year=10){
#
#   # Test params
#   # dataDir <- file.path(getwd(), "data")
#   # plotsFile <- file.path(dataDir, 'SamplePlots.csv')
#   # plots <- read.csv(plotsFile)
#   # plt = plots[1:25,]
#   # fdir = "data/testdata"
#   # map_year=10
#
#   # Refactored version, but conservative (least changes possible to make it work)
#
#   if(!"POINT_X" %in% colnames(plt)){
#     plt$POINT_X <- plt$Xnew
#     plt$POINT_Y <- plt$Ynew
#     plt$PLOT_ID <- 1:nrow(plt)
#   }
#
#   if (class(plt)[1] == 'SpatialPolygonsDataFrame'){
#     plt <- as.data.frame(pt)
#   }
#
#   defo <- c()
#   for (p in 1:nrow(plt)){
#
#     #make a square polygon from plot size
#     xy <- SpatialPoints(plt[p,c('POINT_X', 'POINT_Y')])
#     ch <- chull(plt[p,c('POINT_X', 'POINT_Y')]) #main plot ID needed
#     ww <- ifelse(!(is.na(plt[p,]$SIZE_HA)),
#                  (sqrt(plt[p,]$SIZE_HA*10000) *0.00001)/2, 0.0002) #mean of plots for NAs
#     ww <- ifelse(ww < 0, abs(ww), ww)
#     # > ww
#     # [1] 5e-04
#     pol <- gBuffer(xy, width=ww, quadsegs=1, capStyle="SQUARE") # deprecated
#     # > pol
#     # class       : SpatialPolygons
#     # features    : 1
#     # extent      : 24.53856, 24.53956, 0.9920556, 0.9930556  (xmin, xmax, ymin, ymax)
#     # crs         : NA
#     proj4string(pol) <- CRS('+init=epsg:4326')
#     res <- pol@bbox[4] - pol@bbox[2]
#     # > res
#     # [1] 0.001
#     print(paste('processing:',round((res / 0.00001 * res / 0.00001 ) /10000,2), 'ha'))#checker
#
#     #downloads respective forest loss tile/s from squared plots
#     bb <- unname(bbox(pol))
#     crds <- expand.grid(x=bb[1,],y=bb[2,])
#     fnms <- character(4)
#
#     # > pol
#     # class       : SpatialPolygons
#     # features    : 1
#     # extent      : 24.53856, 24.53956, 0.9920556, 0.9930556  (xmin, xmax, ymin, ymax)
#     # crs         : +proj=longlat +datum=WGS84 +no_defs
#
#     # > bb[1,]
#     # [1] 24.53856 24.53956
#     # > bb[2,]
#     # [1] 0.9920556 0.9930556
#
#     # [,1]       [,2]
#     # [1,] 24.5385556 24.5395556
#     # [2,]  0.9920556  0.9930556
#     # > crds
#     # x         y
#     # 1 24.53856 0.9920556
#     # 2 24.53956 0.9920556
#     # 3 24.53856 0.9930556
#     # 4 24.53956 0.9930556
#     # > fnms
#     # [1] "" "" "" ""
#
#     dir.create(file.path(fdir))
#     #setwd(file.path(fdir))
#     gfcTile <- calc_gfc_tiles(sf::st_as_sf(pol))
#     download_tiles(gfcTile, fdir, images = "lossyear", dataset='GFC-2018-v1.6')
#
#     #get overlapping tile/s (up to 4 possible tiles)
#     for(i in 1:nrow(crds)){
#
#       lon <- 10*(crds[i,1]%/%10)
#       lat <- 10*(crds[i,2]%/%10) + 10
#       LtX <- ifelse(lon < 0, "W", "E")
#       LtY <- ifelse(lat < 0, "S", "N")
#       WE <- paste0(sprintf('%03d',abs(lon)), LtX)
#       NS <- paste0(sprintf('%02d',abs(lat)), LtY)
#
#       fnms[i] <- file.path(fdir, paste0('Hansen_GFC-2018-v1.6_lossyear_',NS, "_", WE,".tif"))
#
#       vls <- numeric()
#       for(f in fnms){
#         if(file.exists(f))
#           vls <- c(vls, extract(raster(f), pol)[[1]])
#         # > vls
#         # [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#       }
#     }
#     vls <- if(length(vls[vls==0]) > length(vls[vls>0])) vls*0 else(c(vls))
#     #if there is more non-deforested (zeros)
#     print(vls)
#     vls[vls>0] <- 1
#
#     defo[p] <- sum(vls[vls>0], na.rm=T) # exact 0 means no deforestation, will return NaN
#     cat(defo[p])
#   }
#
#   plt$defo <- defo
#   thresh <- plt$SIZE_HA * 0.05
#   print(paste('Removed', print(nrow(subset(plt,plt$defo > thresh ))), 'plots that have >5% change'))
#   defPlt <- subset(plt, plt$defo > 0 ) #if there is deforestation
#   defPlt <- subset(defPlt, defPlt$defo <= map_year)#if plot is older or equal to map year
#   netPlt <- dplyr::select(dplyr::setdiff(plt, defPlt),-defo) #removes defo checker column
#   return(list(netPlt, plt)) #returns non-deforested and original plots
# }




# ### SCRIPT TO REMOVE DEFORESTED PLOTS BY OVERLAYING PLOTS TO FOREST LOSS DATA FROM GLOBAL FOREST CHANGE
# ### Hansen et al., 2013
#
# old_Deforested <- function(plt=plt, fdir = flFolder, map_year=10){
#
#   if(!"POINT_X" %in% colnames(plt)){
#     plt$POINT_X <- plt$Xnew
#     plt$POINT_Y <- plt$Ynew
#     plt$PLOT_ID <- 1:nrow(plt)
#   }
#
#   if (class(plt)[1] == 'SpatialPolygonsDataFrame'){
#     plt <- as.data.frame(pt)
#   }
#
#   defo <- c()
#   for (p in 1:nrow(plt)){
#
#     #make a square polygon from plot size
#     xy <- SpatialPoints(plt[p,c('POINT_X', 'POINT_Y')])
#     ch <- chull(plt[p,c('POINT_X', 'POINT_Y')]) #main plot ID needed
#     ww <- ifelse(!(is.na(plt[p,]$SIZE_HA)),
#                  (sqrt(plt[p,]$SIZE_HA*10000) *0.00001)/2, 0.0002) #mean of plots for NAs
#     ww <- ifelse(ww < 0, abs(ww), ww)
#     pol <- gBuffer(xy, width=ww, quadsegs=1, capStyle="SQUARE")
#     proj4string(pol) <- SRS
#     res <- pol@bbox[4] - pol@bbox[2]
#     print(paste('processing:',round((res / 0.00001 * res / 0.00001 ) /10000,2), 'ha'))#checker
#
#     #downloads respective forest loss tile/s from squared plots
#     bb <- unname(bbox(pol))
#     crds <- expand.grid(x=bb[1,],y=bb[2,])
#     fnms <- character(4)
#
#     dir.create(file.path(fdir))
#     setwd(file.path(fdir))
#     gfcTile <- calc_gfc_tiles(pol)
#     download_tiles(gfcTile, fdir, images = "lossyear", dataset='GFC-2018-v1.6')
#
#     #get overlapping tile/s (up to 4 possible tiles)
#     for(i in 1:nrow(crds)){
#
#       lon <- 10*(crds[i,1]%/%10)
#       lat <- 10*(crds[i,2]%/%10) + 10
#       LtX <- ifelse(lon < 0, "W", "E")
#       LtY <- ifelse(lat < 0, "S", "N")
#       WE <- paste0(sprintf('%03d',abs(lon)), LtX)
#       NS <- paste0(sprintf('%02d',abs(lat)), LtY)
#
#       fnms[i] <- file.path(fdir, paste0('Hansen_GFC-2018-v1.6_lossyear_',NS, "_", WE,".tif"))
#
#       vls <- numeric()
#       for(f in fnms){
#         if(file.exists(f))
#           vls <- c(vls, extract(raster(f), pol)[[1]])
#       }
#     }
#     vls <- if(length(vls[vls==0]) > length(vls[vls>0])) vls*0 else(c(vls))
#     #if there is more non-deforested (zeros)
#     print(vls)
#     vls[vls>0] <- 1
#
#     defo[p] <- sum(vls[vls>0], na.rm=T) # exact 0 means no deforestation, will return NaN
#     print(defo[p])
#   }
#
#   plt$defo <- defo
#   thresh <- plt$SIZE_HA * 0.05
#   print(paste('Removed', print(nrow(subset(plt,plt$defo > thresh ))), 'plots that have >5% change'))
#   defPlt <- subset(plt, plt$defo > 0 ) #if there is deforestation
#   defPlt <- subset(defPlt, defPlt$defo <= map_year)#if plot is older or equal to map year
#   netPlt <- dplyr::select(dplyr::setdiff(plt, defPlt),-defo) #removes defo checker column
#   return(list(netPlt, plt)) #returns non-deforested and original plots
# }




# # Testing
#
# # rgeos is deprecated but we need it for testing the old function:
# if(!require(rgeos)){
#   remotes::install_version("rgeos", version = "0.6-4")
#   library(rgeos)
# }
#
# library(testthat)
# library(sf)
# library(terra)
# library(sp)
# library(raster)
# library(gfcanalysis)
#
#
# test_that("Old and new Deforested functions produce consistent results - full sample", {
#
#   # The full sample test (8321 plots) passed, but it took ~3 hours to complete, so we skip it in future testing pipelines
#   # The new function seems about 3.25x more efficient than the older one.
#   #skip_on_ci()
#   skip(message = "Skipping because it takes too long, next test is similar but takes a smaller sample")
#
#   # Test params
#   plots <- read.csv(file.path("data", "SamplePlots.csv"))
#
#   # Run both versions
#   start.time <- Sys.time()
#   old_result <- old_refactored_Deforested(plots, "data/testdata", 10)
#   end.time <- Sys.time()
#   old.time.taken <- end.time - start.time
#   old.time.taken
#   # Time difference of 2.244613 hours
#
#   start.time <- Sys.time()
#   new_result <- Deforested(plots, "data/testdata", 2018, 10)
#   end.time <- Sys.time()
#   new.time.taken <- end.time - start.time
#   new.time.taken
#   # Time difference of 41.50442 mins
#
#   # Compare results
#   expect_equal(dim(old_result[[1]])[1], dim(new_result[[1]])[1])
#   expect_equal(dim(old_result[[2]])[1], dim(new_result[[2]])[1])
#   expect_equal(old_result[[2]]$defo, new_result[[2]]$defo, tolerance = 1e-6)
#
# })
#
#
# test_that("Old and new Deforested functions produce consistent results - small sample", {
#
#   # Test params
#   plots <- read.csv(file.path("data", "SamplePlots.csv"))
#   set.seed(42)
#   plots_sample <- c(sample(1:dim(plots)[1], 10), 182, 200, 323, 6765)
#   sampled_plots <- plots[plots_sample,]
#
#   # Run both versions
#   # Broken old_Deforested() because of deprecated packages and unreproducible environment, we use old_refactored_Deforested() instead
#   # SRS <- CRS('+init=epsg:4326')
#   # old_result <- old_Deforested(plot_data, mock_fdir, 10)
#   old_result <- old_refactored_Deforested(sampled_plots, "data/testdata", 10)
#
#   new_result <- Deforested(sampled_plots, "data/testdata", 2018, 10)
#
#   # Compare results
#   expect_equal(dim(old_result[[1]])[1], dim(new_result[[1]])[1])
#   expect_equal(dim(old_result[[2]])[1], dim(new_result[[2]])[1])
#   expect_equal(old_result[[2]]$defo, new_result[[2]]$defo, tolerance = 1e-6)
#
# })
#
#
# test_that("Deforested function behaves consistently", {
#
#   # Test params
#   plots <- read.csv(file.path("data", "SamplePlots.csv"))
#   set.seed(42)
#   plots_sample <- sample(1:dim(plots)[1], 2)
#   sampled_plots <- plots[plots_sample,]
#
#   result <- Deforested(sampled_plots, "data/testdata")
#
#   # Check output structure
#   expect_type(result, "list")
#   expect_length(result, 2)
#   expect_s3_class(result[[1]], "data.frame")
#   expect_s3_class(result[[2]], "data.frame")
#
#   # Check column names
#   expected_cols <- c("PLOT_ID", "AGB_T_HA", "AVG_YEAR", "geometry", "defo")
#   expect_true(all(expected_cols %in% colnames(result[[2]])))
#
#   # Check deforestation filtering
#   expect_true(all(result[[2]]$defo <= 0.05))
# })
#
#
# test_that("Deforested function defo metric behaves consistently with different GFC dataset years", {
#
#   # Test params
#   plots <- read.csv(file.path("data", "SamplePlots.csv"))
#   set.seed(111)
#   plots_sample <- c(sample(1:dim(plots)[1], 10), 182, 200, 323, 6765)
#   sampled_plots <- plots[plots_sample,]
#
#   result2023 <- Deforested(sampled_plots, "data/testdata", 2023)
#   result2020 <- Deforested(sampled_plots, "data/testdata", 2020)
#   result2015 <- Deforested(sampled_plots, "data/testdata", 2015)
#
#   expect_equal(sum(result2023$plt$defo), 88)
#   expect_equal(sum(result2020$plt$defo), 80)
#   expect_equal(sum(result2015$plt$defo), 40)
#
# })
#
#
# # Test with polygons
# test_that("Deforested function works with polygon input", {
#   # Create sample polygon data
#   poly_data <- st_sf(
#     PLOT_ID = c("P1", "P2"),
#     SIZE_HA = c(1.23, 1.23),
#     geometry = st_sfc(
#       st_polygon(list(1e-3*rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))),
#       st_polygon(list(1e-3*rbind(c(2,2), c(3,2), c(3,3), c(2,3), c(2,2))))
#     ),
#     crs = 4326
#   )
#
#   result <- Deforested(poly_data, "data/testdata")
#
#   # Check output structure for polygon input
#   expect_type(result, "list")
#   expect_length(result, 2)
#   expect_s3_class(result[[1]], "sf")
#   expect_s3_class(result[[2]], "sf")
#
#   # Check if geometry is preserved
#   expect_true("geometry" %in% colnames(result[[1]]))
#   expect_true("geometry" %in% colnames(result[[2]]))
# })


