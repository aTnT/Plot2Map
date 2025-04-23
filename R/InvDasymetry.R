## Updates made to the new framework:
# 13/01/25:
# Replaces raster package functions with terra equivalents.
# Uses sf for spatial operations instead of sp.
# Replaces ddply with base R aggregate function.
# Improves error handling and input validation.
# Uses foreach and doParallel for parallel processing, which are CRAN-compatible.
# 27/02/25:
# Updated function to consider the upgraded sampleAGBmap which now handle multiple AGB datasets
# such as the GEDI L4B and ESACCI datasets
# 13/03/25:
# Improvements in memory (avoiding overloading) and parallel performance (presently 280-350% speed improvement parallel vs sequential):
# - Changed the forest mask and agb raster SpatRaster object inputs to file paths
# - Each worker creates its own SpatRaster object from the file paths, avoiding serialization of external pointers.
# - Converting sf geometries to WKT ensures plot_data is serializable
# Progress bar was added (non-parallel processing)


## Notes:
# Questions:
#   Why:
# 1.
#   } else if (plot_data$SIZE_HA[i] >= 1) {
#     treeCovers <- rep(1, length(threshold))
# 2. Where does this data come from:
# plot_data$AGB_T_HA_UW[i] , plot_data$varPlot ... and other hardcoded columns in plot_data
# if these columns are not present the function fails



#' Inverse dasymetric mapping
#'
#' This function performs inverse dasymetric mapping on plot data. It selects plots
#' based on given criteria, optionally aggregates them, and calculates forest
#' fraction and Above Ground Biomass (AGB) data for each plot or cell.
#'
#' @param plot_data data.frame, Plot dataset containing required columns.
#' @param clmn character, Column name for plot selection.
#' @param value character, Value to select in the specified column.
#' @param aggr numeric, Aggregation factor in degrees. NULL for no aggregation.
#' @param minPlots integer, Minimum number of plots per aggregated cell.
#' @param weighted_mean logical, Whether to use weighted mean for calculations.
#' @param is_poly logical, Whether input plots are polygons.
#' @param parallel logical, Enable parallel processing. Default is FALSE.
#' @param n_cores numeric, Number of cores to use for parallel processing.
#' @param dataset character, Dataset for AGB estimation: "custom", "esacci", or "gedi". Default is "custom".
#' @param agb_raster_path character, File path to the custom AGB raster.
#' @param forest_mask_path character, File path to the forest mask raster.
#' @param threshold numeric, Threshold (0-100) for tree cover calculation and forest masking (e.g. 0 or 10).
#' @inheritParams download_esacci_biomass
#' @inheritParams download_gedi_l4b
#' @inheritParams sampleTreeCover
#'
#' @return A data frame with the following columns:
#'   \item{plotAGB_forestTHs}{AGB values for the forest threshold}
#'   \item{tfPlotAGB}{Tree-filtered plot AGB}
#'   \item{orgPlotAGB}{Original plot AGB}
#'   \item{mapAGB}{AGB from map sampling}
#'   \item{SIZE_HA}{Plot size in hectares}
#'   \item{x}{X-coordinate of plot}
#'   \item{y}{Y-coordinate of plot}
#'   \item{n}{Number of plots (only if aggregated)}
#'
#' @details
#' The function performs the following steps:
#' 1. Selects plots based on the specified criteria.
#' 2. Optionally aggregates plots if `aggr` is not NULL.
#' 3. Calculates forest fraction and AGB for each plot or cell.
#' 4. Samples AGB from custom raster or downloads and uses ESA CCI BIOMASS or GEDI L4B data.
#'
#' @note
#' - Ensure all required columns are present in the input plot data.
#' - For parallel processing, adjust `n_cores` based on your system's capabilities.
#' - Large datasets may require significant processing time and memory.
#'
#' @examples
#' \dontrun{
#'# Create a sample dataset of 10 plots:
#' set.seed(42)
#' sampled_plots <- plots[sample(nrow(plots), 10), ]
#' plot_data <- Deforested(sampled_plots, gfc_folder = test_dir,  map_year = test_year)
#' plot_data <- BiomePair(sampled_plots$non_deforested_plots)
#' plot_data <- TempApply(plot_data, test_year)
#'
#' result <- invDasymetry(plot_data = plot_data,
#'                        clmn = "ZONE",
#'                        value = "Europe",
#'                        aggr = 0.1,
#'                        parallel = TRUE,
#'                        dataset = "esacci")
#' }
#'
#' @import sf
#' @import terra
#' @import foreach
#' @import doParallel
#' @importFrom stats aggregate na.omit weighted.mean
#'
#' @export
invDasymetry <- function(plot_data = NULL, clmn = "ZONE", value = "Europe", aggr = NULL,
                         minPlots = 1, weighted_mean = FALSE, is_poly = TRUE, parallel = FALSE,
                         n_cores = parallel::detectCores() - 1,
                         dataset = "custom", agb_raster_path = NULL, forest_mask_path = NULL,
                         threshold = 0,
                         esacci_biomass_year = "latest", esacci_biomass_version = "latest",
                         esacci_folder = "data/ESACCI-BIOMASS", gedi_l4b_folder = "data/GEDI_L4B/",
                         gedi_l4b_band = "MU", gedi_l4b_resolution = 0.001, timeout = 600) {

  # Limit terra memory usage
  terra::terraOptions(memfrac = 0.5)

  # Input checks
  if (is.null(plot_data)) stop("plot_data cannot be NULL.")
  if (!is.character(dataset) || length(dataset) != 1 || !dataset %in% c("custom", "esacci", "gedi")) {
    stop("dataset must be one of 'custom', 'esacci', or 'gedi'.")
  }

  # Check for required columns when aggr is not NULL
  required_cols <- c("POINT_X", "POINT_Y", "AGB_T_HA_ORIG", "AGB_T_HA", "SIZE_HA", "varPlot")
  if (!is.null(aggr)) {
    missing_cols <- setdiff(required_cols, names(plot_data))
    if (length(missing_cols) > 0 & missing_cols != "varPlot") {
      stop(paste("When aggr is not NULL, plot_data must contain:", paste(missing_cols, collapse = ", ")))
    }
    if (length(missing_cols) > 0 & missing_cols == "varPlot") {
      warning(paste("\"varPlot\" column was not found in the plot_data dataset, it will be estimated"))
    }

    ### TO DO: Call function that adds varPlot to plot_data here
  }


  # Create SpatRaster objects from file paths
  agb_raster <- if (!is.null(agb_raster_path)) terra::rast(agb_raster_path) else NULL
  forest_mask <- if (!is.null(forest_mask_path)) terra::rast(forest_mask_path) else NULL

  # Select plots fulfilling selection criterion
  clm <- which(names(plot_data) == clmn)
  if (length(clm) == 0) stop(paste("Attribute", clmn, "not found"))
  ndx <- which(plot_data[, clm] == value)
  if (length(ndx) == 0) stop("No records satisfy the selection criterion.")
  plot_data <- plot_data[ndx, ]


#
#   # Arnan email 070425:
#
#   # aggregate if aggr != NULL
#   if(!is.null(aggr)){ #if there's agg
#     # aggregate to aggr degree cells
#     plots$Xnew <- aggr * (0.5 + plots$POINT_X %/% aggr)
#     plots$Ynew <- aggr * (0.5 + plots$POINT_Y %/% aggr) #changes XY
#
#     #aggregatioN!
#     #    plots$varTot <- plots$sdTree^2
#
#     plots$inv <- 1/plots$varTot
#     plots$sdMap <- 1 ###since there's a function to estimate SD at 0.1 below
#
#     plotsTMP <- aggregate(plots[,c('AGB_T_HA_ORIG', 'SIZE_HA')],
#                           list(plots$Xnew, plots$Ynew),  mean, na.rm=T) ## AGGREGATION!!!!!!!!!!!!!!!!!
#
#
#     plotsTMP <- cbind(plotsTMP,aggregate(plots[,c('BIO','CODE', 'OPEN', 'VER', 'INVENTORY', 'TIER')],
#                                          list(plots$Xnew, plots$Ynew),   modalClass))
#
#     plotsTMP <- plotsTMP[,-c(5,6)] #remove excess X and Y
#     plotsTMP <- cbind(plotsTMP, aggregate(plots[,"varTot"],
#                                           list(plots$Xnew, plots$Ynew), function(x) 1/sum(1/x))[3])
#
#     plotsTMP <- cbind(plotsTMP, aggregate(plots[,"sdMap"],
#                                           list(plots$Xnew, plots$Ynew), function(x) 1/sum(1/x))[3])
#
#     plotsTMP <- plotsTMP[with(plotsTMP, order(Group.2, Group.1)), ] #order to match
#     x <- ddply(plots, .(paste(plots$Ynew, plots$Xnew)),
#                function(x) data.frame(Xnew=mean(x$Xnew),
#                                       Ynew=mean(x$Ynew),
#                                       AGB_T_HA=weighted.mean(x$AGB_T_HA, x$inv ,na.rm=T)))
#     x <- x[with(x, order(Ynew, Xnew)), ] #order to match
#     tail(x)
#     tail(plotsTMP)
#
#     plotsTMP$AGB_T_HA <- x$AGB_T_HA
#
#     names(plotsTMP) <- c("POINT_X","POINT_Y",'AGB_T_HA_ORIG', 'SIZE_HA', 'BIO',
#                          'CODE', 'OPEN', 'VER', 'INVENTORY','TIER', 'varPlot','varMap','AGB_T_HA')
#
#     # only keep plots satisfying minPlots criterion -- aggregated plots
#     if(minPlots > 1){
#       blockCOUNT <- aggregate(plots[,c("AGB_T_HA")], list(plots$Xnew, plots$Ynew),
#                               function(x) length(na.omit(x)))
#
#       ndx <- which(blockCOUNT$x >= minPlots)
#       plotsTMP1 <- plotsTMP[ndx,]
#       if(nrow(plotsTMP1) < 2){plotsTMP1 <- plotsTMP[1:2,]}
#       plotsTMP1$n <- subset(blockCOUNT,blockCOUNT$x >= minPlots)[[3]] #add plots inside
#       print(plotsTMP1)
#     }
#     plots <- plotsTMP1
#     rsl <- aggr
#   } else {
#     # determine resolution output
#     fname <- list.files(agbTilesFolder, "*.tif")[1]
#     rsl <- xres(raster(file.path(agbTilesFolder, fname)))
#     plots$n <- 1
#     plots$varPlot <- plots$varTot
#     plots$varMap <- plots$sdMap^2
#   }




  # Aggregate if aggr != NULL
  if (!is.null(aggr)) {
    plot_data$Xnew <- aggr * (0.5 + plot_data$POINT_X %/% aggr)
    plot_data$Ynew <- aggr * (0.5 + plot_data$POINT_Y %/% aggr)
    plot_data$inv <- 1 / plot_data$varPlot

    plotsTMP <- aggregate(plot_data[, c("AGB_T_HA_ORIG", "AGB_T_HA", "SIZE_HA")],
                          list(plot_data$Xnew, plot_data$Ynew), mean, na.rm = TRUE)
    plotsTMP <- cbind(plotsTMP, aggregate(plot_data[, "varPlot"],
                                          list(plot_data$Xnew, plot_data$Ynew),
                                          function(x) 1 / sum(1 / x))[3])
    plotsTMP <- plotsTMP[with(plotsTMP, order(Group.2, Group.1)), ]

    x <- aggregate(AGB_T_HA ~ Xnew + Ynew, data = plot_data,
                   FUN = function(x) weighted.mean(x, plot_data$inv[plot_data$AGB_T_HA %in% x], na.rm = TRUE))
    x <- x[with(x, order(Ynew, Xnew)), ]
    plotsTMP$AGB_T_HA1 <- x$AGB_T_HA

    names(plotsTMP) <- c("POINT_X", "POINT_Y", "AGB_T_HA_ORIG", "SIZE_HA", "varPlot", "AGB_T_HA")

    if (minPlots > 1) {
      blockCOUNT <- aggregate(plot_data$AGB_T_HA, list(plot_data$Xnew, plot_data$Ynew),
                              function(x) length(na.omit(x)))
      ndx <- which(blockCOUNT$x >= minPlots)
      plotsTMP1 <- plotsTMP[ndx, ]
      if (nrow(plotsTMP1) < 2) plotsTMP1 <- plotsTMP[1:2, ]
      plotsTMP1$n <- subset(blockCOUNT, blockCOUNT$x >= minPlots)[[3]]
    }
    plot_data <- plotsTMP1
    rsl <- aggr
  } else {
    if (!is.null(agb_raster)) {
      rsl <- terra::res(agb_raster)[1]
    } else if (dataset != "custom") {
      if (dataset == "esacci") {
        fname <- list.files(esacci_folder, "*.tif")[1]
        rsl <- terra::res(terra::rast(file.path(esacci_folder, fname)))[1]
      } else if (dataset == "gedi") {
        fname <- list.files(gedi_l4b_folder, "*.tif")[1]
        rsl <- terra::res(terra::rast(file.path(gedi_l4b_folder, fname)))[1]
      }
    }
  }

  if (nrow(plot_data) <= 1) stop("Too few plots selected; decrease minPlots or run at original resolution")

  library(foreach)

  cat(paste0(nrow(plot_data), " plots being processed...\n"))

  # Define operator for parallel/sequential processing
  `%op%` <- if (parallel) foreach::`%dopar%` else foreach::`%do%`

  # Converting to wkt to use inside worker
  if (is_poly && inherits(plot_data, "sf")) {
    plot_data$geom_wkt <- sf::st_as_text(plot_data$geometry)
  }

  # Parallel setup
  if (parallel) {
    nc <- n_cores
    cl <- parallel::makeCluster(nc)
    doParallel::registerDoParallel(cl)
  }

  export_vars <- c("MakeBlockPolygon", "sampleTreeCover", "sampleAGBmap",
                   "ESACCIAGBtileNames", "download_esacci_biomass", "validate_esacci_biomass_args",
                   "is_poly", "aggr", "plot_data", "rsl", "threshold", "weighted_mean", "dataset",
                   "agb_raster_path", "forest_mask_path",
                   "esacci_biomass_year", "esacci_biomass_version", "esacci_folder",
                   "gedi_l4b_folder", "gedi_l4b_band", "gedi_l4b_resolution", "n_cores", "timeout")

  if (!parallel) {
    n_plots <- nrow(plot_data)
    pb <- txtProgressBar(min = 0, max = n_plots, style = 3)
  }

  FFAGB <- foreach::foreach(
    i = 1:nrow(plot_data),
    .combine = "rbind",
    .packages = c("terra", "sf"),
    .export = if (parallel) export_vars else NULL,
    .errorhandling = "stop"
  ) %op% {

    # Load all package dependencies if developing
    if (Sys.getenv("R_PACKAGE_DEVEL") == "TRUE") {
      devtools::load_all()
    } else {
      library(Plot2Map)
      library(gfcanalysis)
    }

    if (!parallel) setTxtProgressBar(pb, i)

    # Reconstruct geometry or create polygon
    if (is_poly) {
      pol <- sf::st_as_sfg(plot_data$geom_wkt[i])
      pol <- sf::st_sf(geometry = pol, crs = sf::st_crs(plot_data))
    } else {
      pol <- MakeBlockPolygon(plot_data$POINT_X[i], plot_data$POINT_Y[i], rsl)
    }

    # Create local raster objects
    forest_mask_local <- if (!is.null(forest_mask_path)) terra::rast(forest_mask_path) else NULL
    agb_raster_local <- if (!is.null(agb_raster_path)) terra::rast(agb_raster_path) else NULL

    # Clean memory
    if (i %% 50 == 0) gc(full = TRUE)

    if (is.null(aggr)) {
      if (is.na(plot_data$SIZE_HA[i])) {
        treeCovers <- safe_sampleTreeCover(roi = pol, thresholds = threshold, weighted_mean = weighted_mean, forest_mask = forest_mask_local)
      } else if (plot_data$SIZE_HA[i] >= 1) {
        treeCovers <- rep(1, length(threshold))
      } else {
        treeCovers <- safe_sampleTreeCover(roi = pol, thresholds = threshold, weighted_mean = weighted_mean, forest_mask = forest_mask_local)
      }
    } else {
      treeCovers <- safe_sampleTreeCover(roi = pol, thresholds = threshold, weighted_mean = weighted_mean, forest_mask = forest_mask_local)
    }

    wghts2 <- ifelse(is.null(aggr), FALSE, weighted_mean)

    if (!is.null(aggr)) {
      c(treeCovers * plot_data$AGB_T_HA[i], plot_data$AGB_T_HA_ORIG[i],
        # sampleAGBmap(roi = pol, weighted_mean = wghts2, dataset = dataset, agb_raster = agb_raster_local,
        #              esacci_biomass_year = esacci_biomass_year, esacci_biomass_version = esacci_biomass_version,
        #              esacci_folder = esacci_folder, gedi_l4b_folder = gedi_l4b_folder, gedi_l4b_band = gedi_l4b_band,
        #              gedi_l4b_resolution = gedi_l4b_resolution, n_cores = 1, timeout = timeout),
        # plot_data$SIZE_HA[i], plot_data$n[i], plot_data$POINT_X[i], plot_data$POINT_Y[i])
        safe_sampleAGBmap(roi = pol, weighted_mean = wghts2, dataset = dataset, agb_raster = agb_raster_local,
                     esacci_biomass_year = esacci_biomass_year, esacci_biomass_version = esacci_biomass_version,
                     esacci_folder = esacci_folder, gedi_l4b_folder = gedi_l4b_folder, gedi_l4b_band = gedi_l4b_band,
                     gedi_l4b_resolution = gedi_l4b_resolution, n_cores = 1, timeout = timeout),
        plot_data$SIZE_HA[i], plot_data$n[i], plot_data$POINT_X[i], plot_data$POINT_Y[i])
    } else {
      c(treeCovers * plot_data$AGB_T_HA[i], plot_data$AGB_T_HA[i], plot_data$AGB_T_HA_ORIG[i],
        # sampleAGBmap(roi = pol, weighted_mean = wghts2, dataset = dataset, agb_raster = agb_raster_local,
        #              esacci_biomass_year = esacci_biomass_year, esacci_biomass_version = esacci_biomass_version,
        #              esacci_folder = esacci_folder, gedi_l4b_folder = gedi_l4b_folder, gedi_l4b_band = gedi_l4b_band,
        #              gedi_l4b_resolution = gedi_l4b_resolution, n_cores = 1, timeout = timeout),
        safe_sampleAGBmap(roi = pol, weighted_mean = wghts2, dataset = dataset, agb_raster = agb_raster_local,
                     esacci_biomass_year = esacci_biomass_year, esacci_biomass_version = esacci_biomass_version,
                     esacci_folder = esacci_folder, gedi_l4b_folder = gedi_l4b_folder, gedi_l4b_band = gedi_l4b_band,
                     gedi_l4b_resolution = gedi_l4b_resolution, n_cores = 1, timeout = timeout),
        plot_data$SIZE_HA[i], plot_data$POINT_X[i], plot_data$POINT_Y[i])
    }
  }

  if (parallel) parallel::stopCluster(cl)
  FFAGB <- as.data.frame(FFAGB)

  if (!is.null(aggr)) {
    names(FFAGB) <- c(paste0("plotAGB_", "forestTHs"), "orgPlotAGB", "mapAGB", "SIZE_HA", "n", "x", "y")
  } else {
    names(FFAGB) <- c(paste0("plotAGB_", "forestTHs"), "tfPlotAGB", "orgPlotAGB", "mapAGB", "SIZE_HA", "x", "y")
  }

  if (!parallel) close(pb)
  cat("Processing complete. Results: \n\n")
  print(FFAGB)
  return(FFAGB)
}



# Sometimes we get a download error (timeout?), so we run this helpers instead:
safe_sampleAGBmap <- function(roi, weighted_mean, dataset, agb_raster,
                              esacci_biomass_year, esacci_biomass_version,
                              esacci_folder, gedi_l4b_folder,
                              gedi_l4b_band, gedi_l4b_resolution,
                              n_cores, timeout) {
  max_tries <- 3
  wait_time <- 2
  for (try in 1:max_tries) {
    tryCatch({
      result <- sampleAGBmap(roi=roi, weighted_mean=weighted_mean, dataset=dataset, agb_raster=agb_raster,
                             esacci_biomass_year=esacci_biomass_year, esacci_biomass_version=esacci_biomass_version,
                             esacci_folder=esacci_folder, gedi_l4b_folder=gedi_l4b_folder,
                             gedi_l4b_band=gedi_l4b_band,
                             gedi_l4b_resolution=gedi_l4b_resolution,
                             n_cores=n_cores, timeout=timeout)
      return(result)
    }, error=function(e) {
      if (try < max_tries) {
        warning(paste("Error in sampleAGBmap:", e$message, ". Retrying in", wait_time, "seconds."))
        Sys.sleep(wait_time)
      } else {
        stop(paste("sampleAGBmap failed after", max_tries, "tries:", e$message))
      }
    })
  }
}


safe_sampleTreeCover <- function(roi, thresholds, weighted_mean, forest_mask) {
  max_tries <- 3
  wait_time <- 2
  for (try in 1:max_tries) {
    tryCatch({
      result <- sampleTreeCover(roi = roi, thresholds = thresholds, weighted_mean = weighted_mean, forest_mask = forest_mask)
      return(result)
    }, error=function(e) {
      if (try < max_tries) {
        warning(paste("Error in sampleTreeCover:", e$message, ". Retrying in", wait_time, "seconds."))
        Sys.sleep(wait_time)
      } else {
        stop(paste("sampleTreeCover failed after", max_tries, "tries:", e$message))
      }
    })
  }
}



# old_invDasymetry <- function(clmn = "ZONE", value = "Europe", aggr = NULL,
#                              minPlots = 1, wghts = FALSE, is_poly=TRUE, own=TRUE,fmask=NA){
#
#   agbTilesDir <- "data/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2020-fv5.0"
#
#
#   if(class(fmask)=='RasterLayer'){
#     fmask <- fmask
#     plot(fmask)}
#   # returns a data.frame with (mean) AGB from plots satisfying selection
#   # criteria
#
#   if(is.null(aggr)) # overrule minPlots if no aggregagtion
#     minPlots <- 1
#
#   # select plots fulfilling selection criterion set by attr and value
#   clm <- which(names(plots.tf) == clmn)
#   if(length(clm)==0)
#     stop(paste('Attribute', attr, 'not found'))
#
#   ndx <- which(plots.tf[,clm] == value)
#   if(length(ndx)==0)
#     stop('There are no records satisfying the selection criterion.')
#   plots.tf <- plots.tf[ndx,]
#
#   # aggregate if aggr != NULL
#   if(!is.null(aggr)){
#     # aggregate to aggr degree cells
#     plots.tf$Xnew <- aggr * (0.5 + plots.tf$POINT_X %/% aggr)
#     plots.tf$Ynew <- aggr * (0.5 + plots.tf$POINT_Y %/% aggr)
#
#     #aggregatioN!
#     plots.tf$inv <- 1/plots.tf$varPlot
#
#     plotsTMP <- aggregate(plots.tf[,c('AGB_T_HA_ORIG', 'AGB_T_HA', 'SIZE_HA')],
#                           list(plots.tf$Xnew, plots.tf$Ynew),
#                           mean, na.rm=T)
#
#     plotsTMP <- cbind(plotsTMP, aggregate(plots.tf[,"varPlot"],
#                                           list(plots.tf$Xnew, plots.tf$Ynew), function(x) 1/sum(1/x))[3])
#
#     plotsTMP <- plotsTMP[with(plotsTMP, order(Group.2, Group.1)), ] #order to match
#     x <- ddply(plots.tf, .(paste(plots.tf$Ynew, plots.tf$Xnew)),
#                function(x) data.frame(Xnew=mean(x$Xnew),
#                                       Ynew=mean(x$Ynew),
#                                       AGB_T_HA=weighted.mean(x$AGB_T_HA, x$inv ,na.rm=T)))
#     x <- x[with(x, order(Ynew, Xnew)), ]
#
#     plotsTMP$AGB_T_HA1 <- x$AGB_T_HA
#
#     names(plotsTMP) <- c("POINT_X","POINT_Y",'AGB_T_HA_ORIG','AGB_T_HA_UW',
#                          'SIZE_HA', 'varPlot','AGB_T_HA')
#
#     # only keep plots satisfying minPlots criterion
#     if(minPlots > 1){
#       blockCOUNT <- aggregate(plots.tf$AGB_T_HA, list(plots.tf$Xnew, plots.tf$Ynew),
#                               function(x) length(na.omit(x)))
#       ndx <- which(blockCOUNT$x >= minPlots)
#       plotsTMP1 <- plotsTMP[ndx,]
#       if(nrow(plotsTMP1) < 2){plotsTMP1 <- plotsTMP[1:2,]}
#       plotsTMP1$n <- subset(blockCOUNT,blockCOUNT$x >= minPlots)[[3]] #add plots inside
#       print(plotsTMP1)
#     }
#     plots.tf <- plotsTMP1
#     rsl <- aggr
#   } else {
#     # determine resolution output
#     fname <- list.files(agbTilesDir, "*.tif")[99]
#     rsl <- xres(raster(file.path(agbTilesDir, fname)))
#   }
#
#   #error control for few plots after aggregation
#   try(if(nrow(plots.tf) <= 1) stop("very few plots selected, try decreasing minPlots or run at original resolution"))
#
#   print(paste0(nrow(plots.tf), ' number of plots being processed'))
#   if (own==T){rsl <- xres(AGBown)}
#   # sample forest fraction and AGB data per cell/plot
#   nc <- detectCores()
#   cl <- makeCluster(nc-1)
#   registerDoParallel(cl, nc)
#
#
#   FFAGB <- foreach(i=1:nrow(plots.tf), .combine='rbind',# .errorhandling = 'pass',
#                    .packages='raster', .export=c('MakeBlockPolygon', 'SRS',
#                                                  'sampleTreeCover', 'TCtileNames',
#                                                  'AGBtileNames', 'sampleTreeCover',
#                                                  'sampleAGBmap', 'plots', #for polygon
#                                                  'agbTilesDir', 'treeCoverDir', 'AGBown','fmask',
#                                                  'forestTHs')) %dopar% {
#
#                                                    if (is_poly==TRUE){
#                                                      pol <- plots[i,] #own polygon
#                                                    }else{
#                                                      pol <- MakeBlockPolygon(plots.tf$POINT_X[i],
#                                                                              plots.tf$POINT_Y[i], rsl)
#                                                    }
#                                                    if(is.null(aggr)){ #no aggregation!
#                                                      if(is.na(plots.tf$SIZE_HA[i])){
#                                                        treeCovers <- sampleTreeCover(pol, forestTHs, wghts, fmask)
#                                                      } else if(plots.tf$SIZE_HA[i] >= 1){
#                                                        # ***** if plot size equals 1 ha *****
#                                                        treeCovers <- rep(1, length(forestTHs))
#                                                      } else {
#                                                        # ***** if plot size less than 1 ha *****
#                                                        treeCovers <- sampleTreeCover(pol, forestTHs, wghts,fmask)
#                                                      }
#                                                    } else
#                                                      treeCovers <- sampleTreeCover(pol, forestTHs, wghts,fmask)
#                                                    wghts2 <- ifelse(is.null(aggr), FALSE, wghts)
#
#                                                    if(!is.null(aggr)){
#                                                      c(treeCovers * plots.tf$AGB_T_HA[i],
#                                                        plots.tf$AGB_T_HA_UW[i], plots.tf$AGB_T_HA_ORIG[i],
#                                                        sampleAGBmap(pol, wghts2, own),
#                                                        plots.tf$SIZE_HA[i], plots.tf$n[i],
#                                                        plots.tf$POINT_X[i], plots.tf$POINT_Y[i])}
#                                                    else{
#                                                      c(treeCovers * plots.tf$AGB_T_HA[i],plots.tf$AGB_T_HA[i],
#                                                        plots.tf$AGB_T_HA_ORIG[i],
#                                                        sampleAGBmap(pol, wghts2, own),
#                                                        plots.tf$SIZE_HA[i],
#                                                        plots.tf$POINT_X[i], plots.tf$POINT_Y[i])
#                                                    }
#
#                                                  }
#   stopCluster(cl)
#   FFAGB <- data.frame(FFAGB)
#   FFAGB
#   if (!is.null(aggr)){
#     names(FFAGB) <- c(paste0("plotAGB_", forestTHs), "uwPlotAGB", "orgPlotAGB",
#                       "mapAGB",'SIZE_HA', 'n', "x", "y")}
#   else{
#     names(FFAGB) <- c(paste0("plotAGB_", forestTHs), "tfPlotAGB","orgPlotAGB",
#                       "mapAGB",'SIZE_HA', "x", "y")
#   }
#
#
#
#   print(head(FFAGB))
#   return(FFAGB)
# }




# Tests
#
# # # Test comparison between old and new versions
# # test_that("Old and new invDasymetry functions produce consistent results", {
# #   skip("New invDasymetry is now too different than old and cannot be compared")
# #
# #   # Create sample data
# #   plots.tf <- data.frame(
# #     ZONE = c("Europe", "Europe", "Asia"),
# #     POINT_X = c(10, 11, 20),
# #     POINT_Y = c(50, 51, 30),
# #     AGB_T_HA = c(100, 150, 200),
# #     AGB_T_HA_ORIG = c(90, 140, 190),
# #     SIZE_HA = c(1, 0.5, 2),
# #     varPlot = c(10, 15, 20)
# #   )
# #
# #   # Create sample forest mask
# #   fmask <- rast(nrows=10, ncols=10, xmin=0, xmax=20, ymin=20, ymax=60, vals=sample(0:1, 100, replace=TRUE))
# #
# #   # Run both versions
# #   old_result <- old_invDasymetry("ZONE", "Europe", aggr=NULL, minPlots=1, wghts=FALSE, is_poly=FALSE, own=FALSE, fmask=fmask)
# #   new_result <- invDasymetry("ZONE", "Europe", aggr=NULL, minPlots=1, wghts=FALSE, is_poly=FALSE, own=FALSE, fmask=fmask)
# #
# #   # Compare results
# #   expect_equal(old_result, new_result, tolerance=1e-6)
# # })
#
# ## Create test sample dataseet
# # Year for testing
# test_year <- 2023
#
# # Create a test directory if it doesn't exist
# test_dir <- "tests/test_data"
# if (!dir.exists(test_dir)) {
#   dir.create(test_dir,  recursive = TRUE)
# }
#
# # Create a sample dataset of 10 plots:
# set.seed(42)
# sampled_plots <- plots[sample(nrow(plots), 10), ]
# sampled_plots <- Deforested(sampled_plots, gfc_folder = test_dir,  map_year = test_year)
# plot_data <- BiomePair(sampled_plots$non_deforested_plots)
# plot_data <- TempApply(plot_data, test_year)
#
# # Creating a test forest mask
# # Download GLAD 2010 data for each plot:
# glad_tcc_2010 <- lapply(1:dim(plot_data)[1], function(i){
#   download_glad_tcc_2010(
#     roi = plot_data[i,]$geometry,
#     output_folder = test_dir,
#     n_cores = 1,
#     timeout = 1800
#   )
# })
#
#
# # Ensure POINT_Y and POINT_X are present in the sf object or derived from the geometry
# if (!("POINT_Y" %in% names(plot_data)) || !("POINT_X" %in% names(plot_data))) {
#   # Extract coordinates from the geometry column
#   coords_df <- sf::st_coordinates(plot_data)
#
#   # Add the coordinates as new columns to the plot_data sf object
#   plot_data$POINT_X <- coords_df[, "X"]
#   plot_data$POINT_Y <- coords_df[, "Y"]
# }
#
# coords <- unique(paste0(floor(plot_data$POINT_Y), ifelse(plot_data$POINT_Y >= 0, "N", "S"), "_",
#                         sprintf("%03d", floor(abs(plot_data$POINT_X))), ifelse(plot_data$POINT_X >= 0, "E", "W")))
#
# # Call compute_treecover for each unique coordinate
# lapply(coords, function(coord) {
#   compute_treecover(year = test_year,
#                     gfc_folder = test_dir,
#                     #coords = coord,
#                     output_folder = test_dir,
#                     num_cores = 1,
#                     baseline = 2010)
# })
#
#
# fmask_test <- terra::rast(file.path(test_dir,  "GLAD_TCC-2010_treecover_calc_2023_40N_010W.tif"))
#
#
# # Download ESACCI for testing
# esacci_tile_test <- ESACCIAGBtileNames(
#   sf::st_buffer(sampled_plots$non_deforested_plots[3,], 0.0001)$geometry,
#   esacci_biomass_year = "latest",
#   esacci_biomass_version = "latest"
# )
#
# download_esacci_biomass(
#   esacci_folder = test_dir,
#   file_names = esacci_tile_test
# )
#
# agb_raster_test <- terra::rast(file.path(test_dir,  "N40W010_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2021-fv5.0.tif"))
#
# test_that("invDasymetry function tests", {
#
#   # Test case 1: Basic execution with Europe zone, no aggregation
#   result_no_aggr <- invDasymetry(plot_data = plot_data, clmn = "ZONE", value = "Europe", aggr = NULL,
#                                  minPlots = 1, weighted_mean = FALSE, is_poly = FALSE,
#                                  dataset = "custom", agb_raster = agb_raster_test, forest_mask = fmask_test,
#                                  threshold = 10,
#                                  esacci_biomass_year = "latest", esacci_biomass_version = "latest",
#                                  esacci_folder = test_dir,
#                                  gedi_l4b_folder = test_dir,
#                                  gedi_l4b_band = "MU", gedi_l4b_resolution = 0.001,
#                                  n_cores = 1, timeout = 600)
#
#   expect_is(result_no_aggr, "data.frame")
#   expect_gt(nrow(result_no_aggr), 0)
#



