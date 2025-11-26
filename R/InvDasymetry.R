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
# 30/04/2025:
# Fixed column naming to use actual threshold value (e.g., "plotAGB_10" instead of "plotAGB_forestTHs")
# Updated documentation to reflect correct column naming pattern
# 01/05/2025:
# Added automated calculation of varPlot using calculateTotalUncertainty when missing in aggregation
# Improved detection of map_year and map_resolution from available data sources without hardcoding years
# Added proper error messages and warnings for uncertainty calculation steps
# Fixed edge case handling when minPlots <= 1 to prevent "plotsTMP1 not found" error
# Improved cell count handling in aggregation for more robust plot count assignment
# Added handling of edge cases when no cells have enough plots
# 05/05/2025:
# Fixed error "'st_as_sfg' is not an exported object from 'namespace:sf'" by replacing it with st_geometry(st_as_sfc()) combination
# Added NULL check for geom_wkt to prevent errors when working with datasets without WKT geometries
# 06-08/05/2025:
# Added detailed comments explaining the aggregation methodology
# Fixed bug with displaced brackets that lead to difference in aggregated vs non-aggregated case
# Added tests with synthetic data to compare and validate aggregated vs non-aggregated case
# 09/05/2025:
# Fixed critical issue with forest threshold filtering in non-aggregated mode
# Previously the non-aggregated mode was skipping threshold filtering for plots ≥ 1 hectare
# Now always applies threshold filtering consistently in both modes
# 12/05/2025:
# Fixed parameter passing in parallel mode for gfc_folder and gfc_dataset_year
# Created explicit worker versions of helper functions with proper parameter handling
# Added debug logging to track parameter passing in parallel mode
# Ensured consistent function behavior between sequential and parallel execution modes
# 13/05/2025:
# Fixed bug in aggregated mode where weights vector and values vector didn't match in length
# Modified aggregation approach to ensure proper pairing between AGB values and their weights
# This solved the "'x' and 'w' must have the same length" error during weighted mean calculation



#' Inverse dasymetric mapping
#'
#' This function performs inverse dasymetric mapping on plot data. It selects plots
#' based on given criteria, optionally aggregates them, and calculates forest
#' fraction and Above Ground Biomass (AGB) data for each plot or cell. Inverse dasymetric
#' mapping is particularly useful for comparing field inventory plot measurements with remote
#' sensing biomass maps by accounting for differences in spatial scales and forest cover percentages.
#'
#' @param plot_data data.frame, Plot dataset containing required columns. For non-aggregated mode, the
#'        required columns are "POINT_X", "POINT_Y", "AGB_T_HA_ORIG", "AGB_T_HA", and "SIZE_HA".
#'        For aggregated mode (when aggr is not NULL), the "varPlot" column is also required, but will
#'        be automatically calculated if missing.
#' @param clmn character, Column name for plot selection (e.g., "ZONE", "CONTINENT").
#'        Set to NULL to process all plots without filtering (default: "ZONE").
#' @param value character, Value to select in the specified column (e.g., "Europe", "Africa").
#'        Ignored if clmn is NULL (default: "Europe").
#' @param aggr numeric, Aggregation factor in degrees (e.g., 0.1 for 0.1-degree cells). Set to NULL for no aggregation.
#'        When aggregated, plots falling within the same grid cell are combined using inverse variance weighting.
#' @param minPlots integer, Minimum number of plots per aggregated cell. Cells with fewer plots will be excluded.
#' @param is_poly logical, Whether input plots are polygons (TRUE) or should be converted to polygons (FALSE).
#' @param agb_raster_path character, File path to the custom AGB raster.
#' @param forest_mask_path character, File path to the forest mask raster.
#' @param threshold numeric, Threshold (0-100) for tree cover calculation and forest masking (e.g. 0 or 10).
#'  Only pixels with tree cover percentage above this threshold will contribute to biomass estimates.
#' @param map_year numeric, The year of the map data. If not provided, it will be detected automatically from the available data sources.
#' @param map_resolution numeric, The resolution of the map data in degrees. If not provided, it will be detected automatically from the available data sources. Used for variance calculation when aggregating.
#' @param parallel logical, Enable parallel processing for faster computation on multi-core systems. Default is FALSE.
#' @param n_cores numeric, Number of cores to use for parallel processing.
#' @param memfrac numeric, Memory fraction (0-1) for Terra to use in the main process. Default is 0.3.
#' @param worker_memfrac numeric, Memory fraction (0-1) for Terra to use in each worker process during parallel execution (future use - currently fixed at 0.2 internally). Default is 0.2.
#' @param batch_size integer, Number of plots to process in each batch for better memory management. If NULL (default), batch size is auto-determined based on dataset size.
#' @param crop_rasters logical, Whether to crop rasters to the region of interest before processing (TRUE by default).
#' Set to FALSE if you encounter issues with cropping or if the plots are widely dispersed.
#' @inheritParams download_esacci_biomass
#' @inheritParams download_gedi_l4b
#' @inheritParams sampleAGBmap
#' @inheritParams sampleTreeCover
#'
#' @return A data frame with the following columns:
#'   \item{plotAGB_X}{AGB values for the given forest threshold, where X is the threshold value (e.g., plotAGB_10 if threshold=10).
#'     When aggregated, these values are derived from weighted means using inverse variance weighting.
#'     Units are in tonnes per hectare (t/ha).}
#'   \item{tfPlotAGB}{Tree-filtered plot AGB (only when not aggregated). This is equivalent to
#'     the AGB_T_HA values from the input data.}
#'   \item{orgPlotAGB}{Original plot AGB, derived from AGB_T_HA_ORIG in the input data.}
#'   \item{mapAGB}{AGB from map sampling, representing the biomass values extracted from the
#'     reference map at each plot or cell location.}
#'   \item{SIZE_HA}{Plot size in hectares. For aggregated cells, this is the mean plot size within the cell.}
#'   \item{varPlot}{Plot measurement variance (only when aggr is specified and varPlot exists in input data).
#'     Aggregated using inverse variance weighting: 1 / sum(1/varPlot). Units are in (t/ha)².}
#'   \item{n}{Number of plots aggregated per cell (only when aggr is specified).}
#'   \item{x}{X-coordinate of plot or cell center (longitude).}
#'   \item{y}{Y-coordinate of plot or cell center (latitude).}
#'
#' @details
#' The function performs inverse dasymetric mapping through these key steps:
#'
#' 1. **Plot Selection**: Selects plots based on the specified criteria (clmn and value).
#'
#' 2. **Spatial Processing** (two modes):
#'
#'    * **Non-Aggregated Mode** (aggr = NULL): Each plot is processed individually.
#'      - Tree cover percentage is calculated for each plot
#'      - Biomass values are adjusted based on tree cover percentage
#'
#'    * **Aggregated Mode** (e.g., aggr = 0.1): Plots are grouped into grid cells.
#'      - New grid cell coordinates are calculated using integer division
#'      - Plot measurements are aggregated using inverse variance weighting
#'      - Cells with insufficient plot counts (less than minPlots) are filtered out
#'
#' 3. **Forest Cover Correction**:
#'    - Tree cover percentage is calculated for each plot or cell
#'    - Only forest pixels (based on threshold) contribute to biomass estimates
#'
#' 4. **Map Comparison**:
#'    - AGB values are sampled from reference maps
#'    - Enables direct comparison between field-measured and map-estimated biomass
#'
#' @note
#' - Ensure all required columns are present in the input plot data.
#' - For parallel processing, adjust `n_cores` based on your system's capabilities.
#' - Large datasets may require significant processing time and memory.
#' - When using aggregation, the function automatically calculates `varPlot` if missing.
#'
#' @examples
#' \dontrun{
#' # Basic usage with sample data
#' library(Plot2Map)
#' data(plots)
#'
#' # Create a sample dataset and process it
#' set.seed(42)
#' sampled_plots <- plots[sample(nrow(plots), 100), ]
#' plot_data <- Deforested(sampled_plots, gfc_folder = "data/GFC", map_year = 2020)
#' plot_data <- BiomePair(plot_data$non_deforested_plots)
#' plot_data <- TempApplyVar(plot_data, 2020)
#'
#' # Example 1: Non-aggregated mode with custom AGB raster
#' result_individual <- invDasymetry(
#'   plot_data = plot_data,
#'   clmn = "ZONE",
#'   value = "Europe",
#'   aggr = NULL,  # No aggregation
#'   threshold = 10,  # 10% tree cover threshold
#'   dataset = "custom",
#'   agb_raster_path = "path/to/agb_raster.tif"
#' )
#'
#' # Example 2: Aggregated mode with ESA CCI data
#' result_aggregated <- invDasymetry(
#'   plot_data = plot_data,
#'   clmn = "ZONE",
#'   value = "Europe",
#'   aggr = 0.25,  # 0.25° aggregation
#'   minPlots = 2,  # Minimum 2 plots per cell
#'   threshold = 10,
#'   dataset = "esacci",
#'   esacci_biomass_year = "latest"
#' )
#'
#' # Example 3: Process all plots without filtering (no zone selection)
#' result_all_plots <- invDasymetry(
#'   plot_data = plot_data,
#'   clmn = NULL,  # No filtering - process all plots
#'   threshold = 10,
#'   dataset = "esacci"
#' )
#'
#' # Compare plot AGB with map AGB
#' plot(
#'   result_aggregated$plotAGB_10,
#'   result_aggregated$mapAGB,
#'   xlab = "Plot AGB (t/ha)",
#'   ylab = "Map AGB (t/ha)",
#'   main = "Plot vs Map Biomass Comparison"
#' )
#' abline(0, 1, col = "red")  # 1:1 line
#' }
#' @importFrom terra rast values writeRaster vect res terraOptions extract intersect crop ext ncell global
#' @importFrom stats aggregate na.omit weighted.mean quantile
#' @importFrom dplyr filter bind_rows
#' @importFrom graphics hist plot
#' @importFrom sf st_as_text st_as_sfc st_geometry st_crs st_sf st_buffer
#' @importFrom foreach foreach %dopar% %do%
#' @importFrom doParallel registerDoParallel
#'
#' @export
invDasymetry <- function(plot_data = NULL, clmn = "ZONE", value = "Europe", aggr = NULL,
                         minPlots = 1, weighted_mean = FALSE, is_poly = TRUE,
                         dataset = "custom", agb_raster_path = NULL, forest_mask_path = NULL,
                         threshold = 0, map_year = NULL, map_resolution = NULL,
                         esacci_biomass_year = "latest", esacci_biomass_version = "latest",
                         esacci_folder = "data/ESACCI-BIOMASS", gedi_l4b_folder = "data/GEDI_L4B/",
                         gedi_l4b_band = "MU", gedi_l4b_resolution = 0.001,
                         gfc_folder = "data/GFC", gfc_dataset_year = "latest",
                         timeout = 600, parallel = FALSE, n_cores = parallel::detectCores() - 1,
                         memfrac = 0.3, worker_memfrac = 0.2, batch_size = NULL,
                         crop_rasters = TRUE) {

  # Limit terra memory usage - restore original settings on exit
  old_memfrac <- terra::terraOptions()$memfrac
  on.exit(terra::terraOptions(memfrac = old_memfrac))
  terra::terraOptions(memfrac = memfrac)

  # Input checks
  if (is.null(plot_data)) stop("plot_data cannot be NULL.")
  if (!is.character(dataset) || length(dataset) != 1 || !dataset %in% c("custom", "esacci", "gedi")) {
    stop("dataset must be one of 'custom', 'esacci', or 'gedi'.")
  }

  # Check for required columns when aggr is not NULL
  required_cols <- c("POINT_X", "POINT_Y", "AGB_T_HA_ORIG", "AGB_T_HA", "SIZE_HA", "varPlot")
  if (!is.null(aggr)) {
    missing_cols <- setdiff(required_cols, names(plot_data))
    if (length(missing_cols) > 0 && length(setdiff(missing_cols, "varPlot")) > 0) {
      stop(paste("When aggr is not NULL, plot_data must contain:", paste(setdiff(missing_cols, "varPlot"), collapse = ", ")))
    }
    if ("varPlot" %in% missing_cols) {
      warning(paste("\"varPlot\" column was not found in the plot_data dataset, it will be estimated using calculateTotalUncertainty"))

      # Try to determine map_year from data sources if not provided
      if (is.null(map_year)) {
        if (dataset == "esacci") {
          # Extract from esacci_biomass_year
          if (is.numeric(esacci_biomass_year)) {
            map_year <- esacci_biomass_year
          } else if (esacci_biomass_year != "latest") {
            map_year <- as.numeric(esacci_biomass_year)
          } else {
            # Need to determine from file patterns in the folder
            esacci_files <- list.files(esacci_folder, pattern = "*.tif")
            if (length(esacci_files) > 0) {
              # Try to extract year from filenames
              year_pattern <- ".*-(\\d{4})-.*"
              years <- regmatches(esacci_files, regexec(year_pattern, esacci_files))
              years <- unlist(lapply(years, function(x) if(length(x) > 1) x[2] else NULL))
              if (length(years) > 0) {
                map_year <- as.numeric(years[1])
              } else {
                stop("Could not determine map_year from ESA CCI data. Please provide map_year explicitly.")
              }
            } else {
              stop("No ESA CCI files found and map_year not provided. Cannot calculate plot variance.")
            }
          }
        } else if (dataset == "gedi") {
          # For GEDI, use middle of data collection period (2019-2023)
          map_year <- 2021
        } else if (!is.null(agb_raster_path)) {
          # Try to extract year from filename
          year_pattern <- ".*[^0-9](19|20\\d{2})[^0-9].*"
          if (grepl(year_pattern, agb_raster_path)) {
            match <- regexec(".*[^0-9]((?:19|20)\\d{2})[^0-9].*", agb_raster_path)
            year_str <- regmatches(agb_raster_path, match)[[1]][2]
            map_year <- as.numeric(year_str)
          } else {
            stop("Could not determine map_year from data sources. Please provide map_year explicitly.")
          }
        } else {
          stop("Could not determine map_year from data sources. Please provide map_year explicitly.")
        }
      }

      # Try to determine map_resolution from data sources if not provided
      if (is.null(map_resolution)) {
        if (!is.null(agb_raster_path)) {
          # Get resolution from AGB raster
          agb_raster <- terra::rast(agb_raster_path)
          map_resolution <- terra::res(agb_raster)[1]
        } else if (!is.null(forest_mask_path)) {
          # Get resolution from forest mask
          forest_mask <- terra::rast(forest_mask_path)
          map_resolution <- terra::res(forest_mask)[1]
        } else if (dataset == "esacci") {
          # Try to get from an ESA CCI file, or use default
          esacci_files <- list.files(esacci_folder, pattern = "*.tif", full.names = TRUE)
          if (length(esacci_files) > 0) {
            esacci_raster <- terra::rast(esacci_files[1])
            map_resolution <- terra::res(esacci_raster)[1]
          } else {
            # Use default ESA-CCI resolution (0.001 degrees = ~100m at equator)
            map_resolution <- 0.001
            message("Using default ESA-CCI resolution: ", map_resolution, " degrees (~100m)")
          }
        } else if (dataset == "gedi") {
          # Try to get from a GEDI file, or use default
          gedi_files <- list.files(gedi_l4b_folder, pattern = "*.tif", full.names = TRUE)
          if (length(gedi_files) > 0) {
            gedi_raster <- terra::rast(gedi_files[1])
            map_resolution <- terra::res(gedi_raster)[1]
          } else {
            # Use default GEDI resolution
            map_resolution <- gedi_l4b_resolution
            message("Using default GEDI resolution: ", map_resolution, " degrees")
          }
        } else {
          stop("Could not determine map_resolution from data sources. Please provide map_resolution explicitly.")
        }
      }

      message(paste("Using map_year:", map_year, "and map_resolution:", map_resolution, "for uncertainty calculation"))

      # Calculate varPlot using calculateTotalUncertainty
      tryCatch({
        result <- calculateTotalUncertainty(
          plot_data = plot_data,
          map_year = map_year,
          map_resolution = map_resolution,
          biome_info = TRUE
        )

        # Add varPlot to the plot_data
        plot_data$varPlot <- result$data$varPlot

      }, error = function(e) {
        stop(paste("Failed to calculate plot uncertainty:", e$message,
                   "\nPlease provide 'varPlot' column in plot_data."))
      })
    }
  }


  # Create SpatRaster objects from file paths
  agb_raster <- if (!is.null(agb_raster_path)) terra::rast(agb_raster_path) else NULL
  forest_mask <- if (!is.null(forest_mask_path)) terra::rast(forest_mask_path) else NULL

  # Select plots fulfilling selection criterion (skip if clmn is NULL)
  if (!is.null(clmn)) {
    clm <- which(names(plot_data) == clmn)
    if (length(clm) == 0) stop(paste("Attribute", clmn, "not found"))
    ndx <- which(plot_data[, clm] == value)
    if (length(ndx) == 0) stop("No records satisfy the selection criterion.")
    plot_data <- plot_data[ndx, ]
    message("Filtered ", length(ndx), " plots where ", clmn, " == '", value, "'")
  } else {
    message("No filtering applied - processing all ", nrow(plot_data), " plots")
  }


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

    # Creating aggregation cell grid
    plot_data$Xnew <- aggr * (0.5 + plot_data$POINT_X %/% aggr)
    plot_data$Ynew <- aggr * (0.5 + plot_data$POINT_Y %/% aggr)
    plot_data$inv <- 1 / plot_data$varPlot

    # Plot AGB and area are mean aggregated with cell grid
    #plotsTMP <- stats::aggregate(plot_data[, c("AGB_T_HA_ORIG", "AGB_T_HA", "SIZE_HA")], # AGB_T_HA is aggregated below with inv weighted mean
    plotsTMP <- stats::aggregate(plot_data[, c("AGB_T_HA_ORIG", "SIZE_HA")],
                          list(plot_data$Xnew, plot_data$Ynew), mean, na.rm = TRUE)

    # Plot variance is aggregated with inverse of summed variance inversions
    plotsTMP <- cbind(plotsTMP, stats::aggregate(plot_data[, "varPlot"],
                                          list(plot_data$Xnew, plot_data$Ynew),
                                          function(x) 1 / sum(1 / x))[3])
    plotsTMP <- plotsTMP[with(plotsTMP, order(Group.2, Group.1)), ]

    # x <- plyr::ddply(plot_data, .(paste(plot_data$Ynew, plot_data$Xnew)), # removing plyr dependency and replacing with lines below
    #            function(x) data.frame(Xnew=mean(x$Xnew),
    #                                   Ynew=mean(x$Ynew),
    #                                   AGB_T_HA=weighted.mean(x$AGB_T_HA, x$inv ,na.rm=T)))
    # x <- x[with(x, order(Ynew, Xnew)), ]

    # AGB_T_HA is aggregated using inverse variance weighed mean
    # Previous implementation has issue with matching weights to values
    # x <- stats::aggregate(stats::formula(AGB_T_HA ~ Xnew + Ynew), data = plot_data,
    #                FUN = function(x) stats::weighted.mean(x, plot_data$inv[plot_data$AGB_T_HA %in% x], na.rm = TRUE))
    # x <- x[with(x, order(Ynew, Xnew)), ]

    # New implementation - Ensures weights match correctly with values
    # Create a data frame with just the columns needed for aggregation
    agg_data <- data.frame(
      Xnew = plot_data$Xnew,
      Ynew = plot_data$Ynew,
      AGB_T_HA = plot_data$AGB_T_HA,
      inv = plot_data$inv
    )

    # Use an aggregation that preserves the relationship between values and weights
    x <- stats::aggregate(
      cbind(AGB_T_HA, inv) ~ Xnew + Ynew,
      data = agg_data,
      FUN = function(x) if(is.matrix(x)) x else as.numeric(x)
    )

    # Now calculate weighted means for each group using the correctly paired values and weights
    x$weighted_mean <- mapply(function(agb, inv) {
      stats::weighted.mean(agb, inv, na.rm = TRUE)
    }, x$AGB_T_HA, x$inv)

    # Order the result
    x <- x[with(x, order(Ynew, Xnew)), ]
    #plotsTMP$AGB_T_HA <- x$AGB_T_HA

    # Assign the weighted means to plotsTMP
    plotsTMP$AGB_T_HA <- x$weighted_mean

    names(plotsTMP) <- c("POINT_X", "POINT_Y", "AGB_T_HA_ORIG", "SIZE_HA", "varPlot", "AGB_T_HA")

    #   # only keep plots satisfying minPlots criterion
    #   if(minPlots > 1){
    #     blockCOUNT <- aggregate(plot_data$AGB_T_HA, list(plot_data$Xnew, plot_data$Ynew),
    #                             function(x) length(na.omit(x)))
    #     ndx <- which(blockCOUNT$x >= minPlots)
    #     plotsTMP1 <- plotsTMP[ndx,]
    #     if(nrow(plotsTMP1) < 2){plotsTMP1 <- plotsTMP[1:2,]}
    #     plotsTMP1$n <- subset(blockCOUNT,blockCOUNT$x >= minPlots)[[3]] #add plots inside
    #     print(plotsTMP1)
    #   }
    #   plot_data <- plotsTMP1
    #   rsl <- aggr
    # } else {
    #   # determine resolution output
    #   fname <- list.files(agbTilesDir, "*.tif")[99]
    #   rsl <- xres(raster(file.path(agbTilesDir, fname)))
    # }

    # Calculate plot counts per cell for all cells
    blockCOUNT <- stats::aggregate(plot_data$AGB_T_HA, list(plot_data$Xnew, plot_data$Ynew),
                            function(x) length(stats::na.omit(x)))

    if (minPlots > 1) {
      # Filter cells with enough plots
      ndx <- which(blockCOUNT$x >= minPlots)

      if (length(ndx) > 0) {
        # There are cells with enough plots
        plotsTMP1 <- plotsTMP[ndx, ]
        plotsTMP1$n <- subset(blockCOUNT, blockCOUNT$x >= minPlots)[[3]]
      } else {
        # Handle edge case: No cells have enough plots
        stop(paste0("No cells with at least ", minPlots, " plots found. Use a lower 'minPlots', a larger 'aggr' or add more plots to 'plot_data'."))

        #   warning(paste0("No cells with at least ", minPlots, " plots found. Using top cells with most plots."))
        #
        #   # Sort blockCOUNT by number of plots and take top 2 cells
        #   sorted_cells <- blockCOUNT[order(blockCOUNT$x, decreasing = TRUE),]
        #   top_cells <- sorted_cells[1:min(2, nrow(sorted_cells)),]
        #
        #   # Find the rows in plotsTMP corresponding to the top cells
        #   top_cell_indices <- which(paste(plotsTMP$POINT_X, plotsTMP$POINT_Y) %in%
        #                               paste(top_cells$Group.1, top_cells$Group.2))
        #
        #   plotsTMP1 <- plotsTMP[top_cell_indices, ]
        #   plotsTMP1$n <- blockCOUNT$x[top_cell_indices]
        # }
        #
        # # Ensure we have at least 2 cells to avoid errors
        # if (nrow(plotsTMP1) < 2) {
        #   warning("Less than 2 cells available. Using top 2 cells.")
        #   plotsTMP1 <- plotsTMP[1:min(2, nrow(plotsTMP)), ]
        #   plotsTMP1$n <- blockCOUNT$x[1:min(2, nrow(blockCOUNT))]
        # }
        #
      }
      plot_data <- plotsTMP1

    } else {
      # When minPlots <= 1, use plotsTMP directly with plot counts
      plotsTMP$n <- blockCOUNT$x[match(paste(plotsTMP$POINT_X, plotsTMP$POINT_Y),
                                       paste(blockCOUNT$Group.1, blockCOUNT$Group.2))]
      plot_data <- plotsTMP
    }

    # Ensure we have at least 1 cell
    if (nrow(plot_data) < 1) {
      stop("No cells available after aggregation. Check your data and parameters.")
    }

    rsl <- aggr
  }

  if (!is.null(agb_raster)) {
    rsl <- terra::res(agb_raster)[1]
  } else if (dataset != "custom") {
    if (dataset == "esacci") {
      # Check if ESA-CCI tiles exist, otherwise use default resolution
      esacci_files <- list.files(esacci_folder, pattern = "*.tif", full.names = FALSE)
      if (length(esacci_files) > 0) {
        fname <- esacci_files[1]
        rsl <- terra::res(terra::rast(file.path(esacci_folder, fname)))[1]
      } else {
        # Use default ESA-CCI resolution (0.001 degrees = ~100m at equator)
        rsl <- 0.001
        message("No ESA-CCI tiles found in ", esacci_folder,
                ". Using default resolution: ", rsl, " degrees (~100m).",
                "\nTiles will be downloaded automatically as needed.")
      }
    } else if (dataset == "gedi") {
      # Check if GEDI tiles exist, otherwise use default resolution
      gedi_files <- list.files(gedi_l4b_folder, pattern = "*.tif", full.names = FALSE)
      if (length(gedi_files) > 0) {
        fname <- gedi_files[1]
        rsl <- terra::res(terra::rast(file.path(gedi_l4b_folder, fname)))[1]
      } else {
        # Use default GEDI resolution (0.001 degrees)
        rsl <- gedi_l4b_resolution
        message("No GEDI tiles found in ", gedi_l4b_folder,
                ". Using default resolution: ", rsl, " degrees.",
                "\nTiles will be downloaded automatically as needed.")
      }
    }
  }


  if (nrow(plot_data) <= 1) stop("Too few plots selected; decrease minPlots or run at original resolution")

  library(foreach)

  cat(paste0(nrow(plot_data), " plots or cells being processed...\n"))

  # Define operator for parallel/sequential processing
  `%op%` <- if (parallel) `%dopar%` else `%do%`

  # Converting to wkt to use inside worker
  # Handle sf objects efficiently by converting geometry to WKT before parallel execution
  if (is_poly && inherits(plot_data, "sf")) {
    # Convert geometry to WKT for serialisation
    plot_data$geom_wkt <- sf::st_as_text(sf::st_geometry(plot_data))

    # Extract CRS for later use
    crs_orig <- sf::st_crs(plot_data)

    # Convert sf to data frame for better parallel performance
    plot_data <- as.data.frame(sf::st_drop_geometry(plot_data))
    plot_data$crs <- list(crs_orig)  # Store CRS as list column for reconstruction
  }

  # Parallel setup - with optimised resource management
  if (parallel) {
    nc <- n_cores

    # Set terra options before creating the cluster
    # Create cluster with custom configuration
    cl <- parallel::makeCluster(nc)

    # Initialize workers with optimised terra settings
    # Create an environment with only the variables we need to pass to the workers
    worker_env <- new.env()

    # Add our function arguments to this environment
    worker_env$plot_data <- plot_data
    worker_env$is_poly <- is_poly
    worker_env$aggr <- aggr
    worker_env$rsl <- rsl
    worker_env$threshold <- threshold
    worker_env$weighted_mean <- weighted_mean
    worker_env$dataset <- dataset
    worker_env$agb_raster_path <- agb_raster_path
    worker_env$forest_mask_path <- forest_mask_path
    worker_env$esacci_biomass_year <- esacci_biomass_year
    worker_env$esacci_biomass_version <- esacci_biomass_version
    worker_env$esacci_folder <- esacci_folder
    worker_env$gedi_l4b_folder <- gedi_l4b_folder
    worker_env$gedi_l4b_band <- gedi_l4b_band
    worker_env$gedi_l4b_resolution <- gedi_l4b_resolution
    worker_env$gfc_folder <- gfc_folder
    worker_env$gfc_dataset_year <- gfc_dataset_year
    worker_env$timeout <- timeout

    # Export the variables to the workers
    parallel::clusterExport(cl, varlist = names(worker_env), envir = worker_env)

    # Define the helper functions with correct parameters for the worker environment
    worker_safe_sampleTreeCover <- function(roi, thresholds, weighted_mean, forest_mask,
                                         gfc_folder, gfc_dataset_year) {
      max_tries <- 3
      wait_time <- 2

      for (try in 1:max_tries) {
        tryCatch({
          result <- sampleTreeCover(roi = roi, thresholds = thresholds, weighted_mean = weighted_mean,
                                 forest_mask = forest_mask, gfc_folder = gfc_folder,
                                 gfc_dataset_year = gfc_dataset_year)
          return(result)
        }, error=function(e) {
          if (try < max_tries) {
            warning(paste("Error in sampleTreeCover:", e$message, ". Retrying in", wait_time, "seconds."))
            Sys.sleep(wait_time)
            # Force garbage collection between retries
            gc(full = TRUE, verbose = FALSE)
          } else {
            stop(paste("sampleTreeCover failed after", max_tries, "tries:", e$message))
          }
        })
      }
      return(NA)
    }

    worker_safe_sampleAGBmap <- function(roi, weighted_mean, dataset, agb_raster,
                                      esacci_biomass_year, esacci_biomass_version,
                                      esacci_folder, gedi_l4b_folder,
                                      gedi_l4b_band, gedi_l4b_resolution,
                                      n_cores, timeout) {
      max_tries <- 3
      wait_time <- 2

      # Set a lower memory limit for workers when in parallel processing
      if (n_cores > 1) {
        old_memfrac <- terra::terraOptions()$memfrac
        on.exit(terra::terraOptions(memfrac = old_memfrac), add = TRUE)
        terra::terraOptions(memfrac = 0.2)
      }

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
            # Force garbage collection between retries
            gc(full = TRUE, verbose = FALSE)
          } else {
            stop(paste("sampleAGBmap failed after", max_tries, "tries:", e$message))
          }
        })
      }
      return(NA)
    }

    # Export the worker versions of the helper functions
    parallel::clusterExport(cl, c("worker_safe_sampleTreeCover", "worker_safe_sampleAGBmap"), envir = environment())

    # Export MakeBlockPolygon from the package namespace
    # In development mode, we need to use a different approach
    if (Sys.getenv("R_PACKAGE_DEVEL") == "TRUE") {
      # In development mode with devtools::load_all(), functions are in the global environment
      parallel::clusterExport(cl, c("MakeBlockPolygon", "sampleTreeCover", "sampleAGBmap"),
                              envir = .GlobalEnv)
    } else {
      # In production mode, functions are in the package namespace
      parallel::clusterExport(cl, c("MakeBlockPolygon", "sampleTreeCover", "sampleAGBmap"),
                              envir = asNamespace("Plot2Map"))
    }

    # Make 'parallel' variable available to workers
    parallel::clusterExport(cl, "parallel", envir = environment())

    parallel::clusterEvalQ(cl, {
      # Configure terra to use less memory per worker (fixed value)
      terra::terraOptions(memfrac = 0.2)

      # Load required libraries
      library(terra)
      library(sf)
      library(stats)

      # Try to load Plot2Map package if installed (for regular use)
      # Will be silently skipped during development with devtools::load_all()
      tryCatch({
        library(Plot2Map)
      }, error = function(e) {
        # Skip loading if package not installed - this is expected during development
      })

      # Clear any cached GFC tiles grid to ensure fresh calculation
      # This is important because tile intersection logic may have been updated
      if (exists("gfc_tiles_grid", envir = .GlobalEnv)) {
        rm("gfc_tiles_grid", envir = .GlobalEnv)
      }

      # Set up worker environment
      options(warn = 1)  # Show warnings immediately

      # Return status
      TRUE
    })

    doParallel::registerDoParallel(cl)
  }

  # No need for export_vars anymore since we're using clusterExport directly

  # Define batch size for better memory management in parallel mode
  if (is.null(batch_size)) {
    # Auto determine batch size if not specified
    batch_size <- ifelse(parallel,
                      ifelse(nrow(plot_data) > 1000, 50,
                        ifelse(nrow(plot_data) > 500, 100, 200)),
                      nrow(plot_data))  # No batching in sequential mode
  } else if (!parallel && batch_size < nrow(plot_data)) {
    # In sequential mode, default to processing all at once unless batch_size is explicitly set
    message("Using batch processing in sequential mode with batch size: ", batch_size)
  }

  # Calculate number of batches
  total_batches <- ceiling(nrow(plot_data) / batch_size)

  # Use consistent progress reporting for both parallel and sequential mode
  cat(paste0("Processing ", nrow(plot_data), " plots in ", total_batches,
            " batches of up to ", batch_size, " plots each\n"))

  # Process in batches to manage memory better
  FFAGB <- foreach::foreach(
    batch_idx = 1:total_batches,
    .combine = "rbind",
    .packages = c("terra", "sf", "stats"),
    .export = NULL,  # Don't export variables explicitly as they're already available in the environment
    .errorhandling = "stop",
    .inorder = TRUE  # Preserve the original order of plots
  ) %op% {

    # Calculate batch start and end indices
    start_idx <- (batch_idx - 1) * batch_size + 1
    end_idx <- min(batch_idx * batch_size, nrow(plot_data))
    batch_indices <- start_idx:end_idx

    # Process the entire batch
    batch_results <- do.call(rbind, lapply(batch_indices, function(i) {

      # Load all package dependencies if developing
      if (Sys.getenv("R_PACKAGE_DEVEL") == "TRUE") {
        suppressMessages(devtools::load_all())
      } else {
        suppressMessages(library(Plot2Map))
      }

      # # Print progress by batch instead of by plot
      # if (batch_idx %% max(1, ceiling(total_batches/20)) == 0) {
      #   completed_percent <- round(batch_idx * 100 / total_batches)
      #   cat(sprintf("\rProgress: %d%% complete...", completed_percent))
      #   if (batch_idx == total_batches) cat("\n")
      # }

      # Reconstruct geometry or create polygon
      if (is_poly) {
        if (!is.null(plot_data$geom_wkt) && !is.na(plot_data$geom_wkt[i])) {
          tryCatch({
            # Reconstruct geometry from WKT
            pol_geom <- sf::st_geometry(sf::st_as_sfc(plot_data$geom_wkt[i]))

            # Use stored CRS if available, otherwise use default CRS
            if (!is.null(plot_data$crs) && length(plot_data$crs) >= i) {
              pol <- sf::st_sf(geometry = pol_geom, crs = plot_data$crs[[i]])
            } else {
              pol <- sf::st_sf(geometry = pol_geom, crs = 4326) # Default to WGS84
            }
          }, error = function(e) {
            # Fall back to block polygon if reconstruction fails
            warning(paste("Failed to reconstruct geometry from WKT for plot", i, ":", e$message))
            pol <- MakeBlockPolygon(plot_data$POINT_X[i], plot_data$POINT_Y[i], rsl)
          })
        } else {
          # Fall back to block polygon if WKT is not available
          pol <- MakeBlockPolygon(plot_data$POINT_X[i], plot_data$POINT_Y[i], rsl)
        }
      } else {
        pol <- MakeBlockPolygon(plot_data$POINT_X[i], plot_data$POINT_Y[i], rsl)
      }

      # Create local raster objects with optimized memory settings
      if (!is.null(forest_mask_path)) {
        if (crop_rasters) {
          # Create with lower memory footprint and only the area we need
          roi_bbox <- sf::st_bbox(pol)
          # Add buffer to ensure we get all needed cells
          roi_bbox <- roi_bbox + c(-rsl, -rsl, rsl, rsl)

          # Load only the necessary extent
          forest_mask_local <- terra::rast(forest_mask_path)
          ext_crop <- terra::ext(roi_bbox[c(1, 3, 2, 4)])
          if (!is.null(forest_mask_local)) {
            tryCatch({
              # First try to crop to the ROI extent
              forest_mask_local <- terra::crop(forest_mask_local, ext_crop)
            }, error = function(e) {
              # If cropping fails, use the full raster
              warning("Cropping forest mask failed, using full raster")
              forest_mask_local <- terra::rast(forest_mask_path)
            })
          }
        } else {
          # Load full raster without cropping
          forest_mask_local <- terra::rast(forest_mask_path)
        }
      } else {
        forest_mask_local <- NULL
      }

      if (!is.null(agb_raster_path)) {
        if (crop_rasters) {
          # Create with lower memory footprint and only the area we need
          roi_bbox <- sf::st_bbox(pol)
          # Add buffer to ensure we get all needed cells
          roi_bbox <- roi_bbox + c(-rsl, -rsl, rsl, rsl)

          # Load only the necessary extent
          agb_raster_local <- terra::rast(agb_raster_path)
          ext_crop <- terra::ext(roi_bbox[c(1, 3, 2, 4)])
          if (!is.null(agb_raster_local)) {
            tryCatch({
              # First try to crop to the ROI extent
              agb_raster_local <- terra::crop(agb_raster_local, ext_crop)
            }, error = function(e) {
              # If cropping fails, use the full raster
              warning("Cropping AGB raster failed, using full raster")
              agb_raster_local <- terra::rast(agb_raster_path)
            })
          }
        } else {
          # Load full raster without cropping
          agb_raster_local <- terra::rast(agb_raster_path)
        }
      } else {
        agb_raster_local <- NULL
      }

      # Always use sampleTreeCover to apply consistent threshold filtering
      # In parallel mode, use the worker version of the function
      if (parallel) {
        treeCovers <- worker_safe_sampleTreeCover(roi = pol, thresholds = threshold,
                                             weighted_mean = weighted_mean, forest_mask = forest_mask_local,
                                             gfc_folder = gfc_folder, gfc_dataset_year = gfc_dataset_year)
      } else {
        treeCovers <- safe_sampleTreeCover(roi = pol, thresholds = threshold,
                                         weighted_mean = weighted_mean, forest_mask = forest_mask_local,
                                         gfc_folder = gfc_folder, gfc_dataset_year = gfc_dataset_year)
      }

      wghts2 <- ifelse(is.null(aggr), FALSE, weighted_mean)

      # Clean up raster objects once we're done with them to free memory
      result_row <- if (!is.null(aggr)) {
        # Aggregated mode
        # In parallel mode, use the worker version of the function
        agb_value <- if (parallel) {
            worker_safe_sampleAGBmap(roi = pol, weighted_mean = wghts2, dataset = dataset,
                              agb_raster = agb_raster_local,
                              esacci_biomass_year = esacci_biomass_year,
                              esacci_biomass_version = esacci_biomass_version,
                              esacci_folder = esacci_folder, gedi_l4b_folder = gedi_l4b_folder,
                              gedi_l4b_band = gedi_l4b_band,
                              gedi_l4b_resolution = gedi_l4b_resolution, n_cores = 1,
                              timeout = timeout)
        } else {
            safe_sampleAGBmap(roi = pol, weighted_mean = wghts2, dataset = dataset,
                              agb_raster = agb_raster_local,
                              esacci_biomass_year = esacci_biomass_year,
                              esacci_biomass_version = esacci_biomass_version,
                              esacci_folder = esacci_folder, gedi_l4b_folder = gedi_l4b_folder,
                              gedi_l4b_band = gedi_l4b_band,
                              gedi_l4b_resolution = gedi_l4b_resolution, n_cores = 1,
                              timeout = timeout)
        }
        res <- c(treeCovers * plot_data$AGB_T_HA[i], plot_data$AGB_T_HA_ORIG[i],
                 agb_value, plot_data$SIZE_HA[i], plot_data$varPlot[i],
                 plot_data$n[i], plot_data$POINT_X[i], plot_data$POINT_Y[i])
        res
      } else {
        # Non-aggregated mode
        # In parallel mode, use the worker version of the function
        agb_value <- if (parallel) {
            worker_safe_sampleAGBmap(roi = pol, weighted_mean = wghts2, dataset = dataset,
                              agb_raster = agb_raster_local,
                              esacci_biomass_year = esacci_biomass_year,
                              esacci_biomass_version = esacci_biomass_version,
                              esacci_folder = esacci_folder, gedi_l4b_folder = gedi_l4b_folder,
                              gedi_l4b_band = gedi_l4b_band,
                              gedi_l4b_resolution = gedi_l4b_resolution, n_cores = 1,
                              timeout = timeout)
        } else {
            safe_sampleAGBmap(roi = pol, weighted_mean = wghts2, dataset = dataset,
                              agb_raster = agb_raster_local,
                              esacci_biomass_year = esacci_biomass_year,
                              esacci_biomass_version = esacci_biomass_version,
                              esacci_folder = esacci_folder, gedi_l4b_folder = gedi_l4b_folder,
                              gedi_l4b_band = gedi_l4b_band,
                              gedi_l4b_resolution = gedi_l4b_resolution, n_cores = 1,
                              timeout = timeout)
        }
        res <- c(treeCovers * plot_data$AGB_T_HA[i], plot_data$AGB_T_HA[i],
              plot_data$AGB_T_HA_ORIG[i], agb_value,
              plot_data$SIZE_HA[i], plot_data$POINT_X[i], plot_data$POINT_Y[i])
        res
      }

      # Clean up to free memory
      if (exists("forest_mask_local") && !is.null(forest_mask_local)) {
        rm(forest_mask_local)
      }
      if (exists("agb_raster_local") && !is.null(agb_raster_local)) {
        rm(agb_raster_local)
      }

      # Return the result row
      return(result_row)
    }))

    # Clean up memory after processing each batch
    gc(full = TRUE, verbose = FALSE)

    # Return the batch results
    return(batch_results)
  }

  if (parallel) parallel::stopCluster(cl)
  FFAGB <- as.data.frame(FFAGB)

  if (!is.null(aggr)) {
    names(FFAGB) <- c(paste0("plotAGB_", threshold), "orgPlotAGB", "mapAGB", "SIZE_HA", "varPlot", "n", "x", "y")
  } else {
    names(FFAGB) <- c(paste0("plotAGB_", threshold), "tfPlotAGB", "orgPlotAGB", "mapAGB", "SIZE_HA", "x", "y")
  }

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

  # Set a lower memory limit for workers when in parallel processing
  if (n_cores > 1) {
    old_memfrac <- terra::terraOptions()$memfrac
    on.exit(terra::terraOptions(memfrac = old_memfrac), add = TRUE)  # Use add=TRUE to avoid overriding the previous on.exit
    # Use a fixed conservative value for memory fraction (0.2)
    terra::terraOptions(memfrac = 0.2)
  }

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
        # Force garbage collection between retries
        gc(full = TRUE, verbose = FALSE)
      } else {
        stop(paste("sampleAGBmap failed after", max_tries, "tries:", e$message))
      }
    })
  }

  # This will never be reached but is here for code completeness
  return(NA)
}


safe_sampleTreeCover <- function(roi, thresholds, weighted_mean, forest_mask,
                            gfc_folder, gfc_dataset_year) {
  max_tries <- 3
  wait_time <- 2

  for (try in 1:max_tries) {
    tryCatch({
      result <- sampleTreeCover(roi = roi, thresholds = thresholds, weighted_mean = weighted_mean,
                               forest_mask = forest_mask, gfc_folder = gfc_folder,
                               gfc_dataset_year = gfc_dataset_year)
      return(result)
    }, error=function(e) {
      if (try < max_tries) {
        warning(paste("Error in sampleTreeCover:", e$message, ". Retrying in", wait_time, "seconds."))
        Sys.sleep(wait_time)
        # Force garbage collection between retries
        gc(full = TRUE, verbose = FALSE)
      } else {
        stop(paste("sampleTreeCover failed after", max_tries, "tries:", e$message))
      }
    })
  }

  # This will never be reached but is here for code completeness
  return(NA)
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

