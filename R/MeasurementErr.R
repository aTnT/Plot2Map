## Updates made to the new framework:
# 20/01/25:
# Minor changes to make code more robust to eventual changes in the BIOMASS uptream package
# 07/04/25:
# Updated documentation
# 25/04/2025:
# Added calculateTotalUncertainty that:
# - Automatically detects the plot data type (tree-level, point, polygon, nested, lidar)
# - Preserves existing uncertainty values if present
# - Calculates measurement uncertainty for non-tree-level data using RF model
# - Implements the Rejou-Mechain sampling error calculation
# - Uses biome-specific growth uncertainty from TempVar
# - Combines all components into total uncertainty
# 29/04/2025:
# Bug fix:
# 1. Make sure the ranger package is properly loaded using requireNamespace("ranger")
# 2. Use the S3 generic method predict() instead of ranger::predict() since the ranger package uses S3 methods for
# predict functionality
# 3. Access the results using $predictions from the prediction object returned by ranger


#' Calculate AGB and standard deviations from tree-level data
#'
#' This function calculates plot-level Above Ground Biomass (AGB) and Standard Deviation using tree-level data and plot locations.
#' It uses the BIOMASS package for AGB calculation and standard deviation estimation.
#'
#' @param plot A data frame containing tree-level data. Must include the following columns:
#'   \describe{
#'     \item{id}{Unique identifier for each tree or plot (character or numeric).}
#'     \item{diameter}{Diameter at breast height (DBH) of trees in centimeters (numeric).}
#'     \item{genus}{Genus of the tree species (character).}
#'     \item{species}{Species of the tree (character).}
#'     \item{height}{Tree height in meters (optional, numeric).}
#'   }
#'
#' @param xy A data frame containing plot location data. Must include the following columns:
#'   \describe{
#'     \item{id}{Unique identifier for each plot (character or numeric).}
#'     \item{x}{X-coordinate of the plot location (numeric).}
#'     \item{y}{Y-coordinate of the plot location (numeric).}
#'     \item{size}{Plot size in square meters (numeric).}
#'     \item{year}{Year of measurement or survey (numeric).}
#'   }
#'
#' @inheritParams BIOMASS::getWoodDensity
#'
#' @return A data frame with plot-level AGB estimates and standard deviations, including columns:
#'   \describe{
#'     \item{PLOT_ID}{Unique identifier for each plot.}
#'     \item{POINT_X}{X-coordinate of the plot location.}
#'     \item{POINT_Y}{Y-coordinate of the plot location.}
#'     \item{SIZE_HA}{Plot size in hectares.}
#'     \item{AVG_YEAR}{Average year of measurement across trees in the plot.}
#'     \item{AGB_T_HA}{Above Ground Biomass scaled to tons per hectare.}
#'     \item{sdTree}{Standard deviation of AGB within the plot.}
#'   }
#'
#' @details
#' The function performs the following steps:
#' 1. Filters trees with diameter >= 10cm.
#' 2. Corrects taxonomy and retrieves wood density data.
#' 3. Computes or uses provided tree height data.
#' 4. Runs Monte Carlo simulation for AGB estimation.
#' 5. Calculates plot-level AGB and standard deviation.
#' 6. Scales values per hectare.
#'
#' @import dplyr
#' @importFrom BIOMASS correctTaxo getWoodDensity modelHD retrieveH AGBmonteCarlo
#'
#' @examples
#' plotsTree <- utils::read.csv(sample_file("SampleTree.csv"))
#' head(plotsTree)
#'
#' xyTree <- utils::read.csv(sample_file("SampleTreeXY.csv"))
#' head(xyTree)
#'
#' plot_uncertainties <- MeasurementErr(plotsTree, xyTree, region = "India")
#' head(plot_uncertainties)
#'
#' @export
sd_tree <- function(plot, xy, region = "World") {

  plot <- subset(plot, diameter>=10) #filter those above 10cm in diameter
  #  blowup <- plot[1,5] / 10000
  # print(paste('plot size is', blowup, 'ha'))

  #taxonomy correction
  #tax <- correctTaxo(genus = plot$genus, species = plot$species)
  #plot$genus <- tax$genusCorrected
  #plot$species <- tax$speciesCorrected

  #get wood density
  wd <- BIOMASS::getWoodDensity(genus = plot$genus,
                                species = plot$species,
                                stand = plot$id, region=region)
  plot$wd <- wd$meanWD
  plot$sd.wd<- wd$sdWD

  #compute local HD model / input your own H data if you have
  if("height" %in% colnames(plot)){
    message('Using actual tree height from the provided plot data.')
  }else{
    message('No tree height data found in original plot data. Calculating height using BIOMASS height-diameter model.')
    HDmodel <- modelHD(D = BIOMASS::NouraguesHD$D, H = BIOMASS::NouraguesHD$H,
                       method='weibull',  useWeight = TRUE, drawGraph = FALSE, plot = NULL)
    dataHlocal <- retrieveH(D = plot$diameter, model = HDmodel)
    plot$height <- dataHlocal$H
  }

  #run MC simulation
  if("height" %in% colnames(plot)){
    mc <- by(plot, plot$id,
             function(x) BIOMASS::AGBmonteCarlo(D = x$diameter, WD = x$wd, errWD = x$sd.wd,
                                       H = x$height, errH = x$height, Dpropag ='chave2004'),simplify = F)
  }else{
    mc <- by(plot, plot$id,
             function(x) BIOMASS::AGBmonteCarlo(D = x$diameter, WD = x$wd, errWD = x$sd.wd,
                                       HDmodel = HDmodel, Dpropag = "chave2004"),simplify = F)}

  #get agb and sd
  agb <- unlist(sapply(mc, "[", "meanAGB"))
  sd <- unlist(sapply(mc, "[", "sdAGB"))

  #add XY
  #plot.fin <- left_join(plot, xy, by = c('id' = 'id')) #needs full to avoid gaps
  plot.fin <- dplyr::left_join(plot, unique(xy), by = dplyr::join_by(id))

  #remove unecessaries
  plot.fin <- plot.fin[,c("id","x","y", 'size', 'year')] # retain columns of interest

  #summarize per plot and add key results
  plot.fin$x <- as.numeric(plot.fin$x)
  plot.fin$y <- as.numeric(plot.fin$y)

  plot.fin <- plot.fin |>
    dplyr::group_by(id) |>
    dplyr::summarise_all(mean)

  #scale values per ha
  agb <- agb / (plot.fin$size/10000)
  sd <- sd / (plot.fin$size/10000)
  plot.fin$agb <- agb
  plot.fin$sd <- sd
  plot.fin <- as.data.frame(plot.fin[,c("id","x","y", 'size', 'year', 'agb', 'sd')]) # retain columns of interest
  plot.fin$size <- plot.fin$size / 10000
  names(plot.fin) <- c('PLOT_ID', 'POINT_X', 'POINT_Y', 'SIZE_HA', 'AVG_YEAR',
                       'AGB_T_HA', 'sdTree')
  return(plot.fin)

}



#' Comprehensive uncertainty calculation for all plot data types
#'
#' This function provides a unified framework for calculating uncertainty across different plot data types.
#' It automatically detects the plot data type, preserves existing uncertainty components if present,
#' and calculates missing components as needed.
#'
#' @param plot_data A data frame containing plot data
#' @param map_year Numeric value indicating the map year for temporal adjustment
#' @param map_resolution Numeric value indicating the map resolution in meters (default: 100)
#' @param biome_info Logical indicating whether to use biome information for growth uncertainty (default: TRUE)
#' @return A list containing:
#'   \describe{
#'     \item{data}{The plot data with calculated uncertainty components}
#'     \item{plot_type}{The detected plot data type}
#'     \item{uncertainty_components}{The relative contribution of each uncertainty component}
#'   }
#' @export
#' @examples
#' # Create a sample dataset of 10 plots:
#' set.seed(42)
#' sampled_plots <- plots[sample(nrow(plots), 10), ]
#' # For non-tree level data
#' sampled_plots2 <- BiomePair(sampled_plots)
#' print(sampled_plots2)
#'
#' uncertainty_results <- calculateTotalUncertainty(sampled_plots2, 2020, 100)
#' print(uncertainty_results)
#'
#' # For tree-level data with sdTree already calculated
#' plots_tree <- sd_tree(plotTree, xyTree, "Asia")
#' uncertainty_results <- calculateTotalUncertainty(plots_tree, 2020, 100)
calculateTotalUncertainty <- function(plot_data, map_year, map_resolution = 100, biome_info = TRUE) {

  # Automatically determine plot type based on data structure and columns
  plot_type <- determineDataType(plot_data)

  # 1. Handle measurement error based on plot type and check for existing uncertainty data
  if ("sdTree" %in% names(plot_data)) {
    # If sdTree already exists, preserve it
    message(paste0("Using existing sdTree values for ", plot_type, " data"))
  } else {
    # No existing sdTree, calculate based on plot type
    if (plot_type %in% c("tree_level", "nested", "lidar")) {
      stop(paste0(plot_type, " data requires sdTree column. Run MeasurementErr() first"))
    } else {
      # For cases without tree-level data (cases 1-3), use RF model
      message("Calculating tree measurement uncertainty using RF model")

      # Check if required columns exist
      if (!all(c("AGB_T_HA", "SIZE_HA", "GEZ") %in% names(plot_data))) {
        stop("Required columns missing: AGB_T_HA, SIZE_HA, or GEZ")
      }

      plotsPred <- plot_data[, c('AGB_T_HA', 'SIZE_HA', 'GEZ')]
      names(plotsPred) <- c('agb', 'size', 'gez')
      plotsPred$size <- as.numeric(plotsPred$size) * 10000
      plotsPred$gez <- factor(plotsPred$gez,
                              levels = c("Boreal", "Subtropical", "Temperate", "Tropical"))

      # Load RF model from package data
      rf1_path <- sample_file("rf1.RData")
      if (!file.exists(rf1_path)) {
        stop("RF model file 'rf1.RData' not found in package data. Please ensure the package is properly installed.")
      }

      # Try to load the model
      tryCatch({
        load(rf1_path)
        # Make sure ranger package is loaded
        if (!requireNamespace("ranger", quietly = TRUE)) {
          stop("The ranger package is needed but not installed")
        }
        # Ensure the ranger package is loaded for S3 methods to work
        requireNamespace("ranger")
        # Predict measurement uncertainty (directly using S3 method)
        predictions <- predict(rf1, data = plotsPred)
        plot_data$sdTree <- predictions$predictions
      }, error = function(e) {
        stop("Error loading or using RF model: ", e$message)
      })
    }
  }

  # 2. Calculate or preserve sampling uncertainty using Rejou-Mechain approach
  if ("sdSE" %in% names(plot_data)) {
    message("Using existing sampling uncertainty (sdSE) values")
  } else {
    message("Calculating sampling uncertainty using Rejou-Mechain approach")

    # Calculate map resolution in hectares
    plot_data$RS_HA <- map_resolution^2 / 10000

    # Calculate size ratio (plot size / pixel size)
    plot_data$ratio <- as.numeric(plot_data$SIZE_HA) / plot_data$RS_HA

    # Try to load sampling error model
    if ("rfSE" %in% ls(envir = .GlobalEnv)) {
      # Use existing model in global environment
      rfSE <- get("rfSE", envir = .GlobalEnv)
    } else {

      # Load se.csv from package data
      se_file <- sample_file("se.csv")
      if (!file.exists(se_file)) {
        stop("Sampling error data file 'se.csv' not found in package data. Please ensure the package is properly installed.")
      }

      # # Try to load the model
      # tryCatch({
      #   load(rf1_path)
      #   # Predict measurement uncertainty
      #   plot_data$sdTree <- predict(rf1, plotsPred)$predictions
      # }, error = function(e) {
      #   stop("Error loading or using RF model: ", e$message)
      # })
      #
      #
      #
      # # Try to load the sampling error data
      # se_file <- tryCatch({
      #   #file.path(getOption("dataDir", "."), "se.csv")
      #   # Load se.csv from package data
      #   se_file <- sample_file("se.csv")
      #   if (!file.exists(se_file)) {
      #     stop("Sampling error data file 'se.csv' not found in package data. Please ensure the package is properly installed.")
      #   }
      # }, error = function(e) {
      #   system.file(se_file, package = "Plot2Map")
      # })

      if (file.exists(se_file)) {
        # Make sure ranger package is loaded
        if (!requireNamespace("ranger", quietly = TRUE)) {
          stop("The ranger package is needed but not installed")
        }
        se <- read.csv(se_file)
        rfSE <- ranger::ranger(se$cv ~ ., data = se[, c('SIZE_HA', 'RS_HA', 'ratio')])
      } else {
        stop("Cannot find sampling error data (se.csv). Please specify correct path or provide rfSE model")
      }
    }

    # Predict sampling error using random forest model
    # Convert to standard deviation by multiplying by mean AGB
    # Ensure the ranger package is loaded for S3 methods to work
    requireNamespace("ranger")
    rfSE_pred <- predict(rfSE, data = plot_data[, c('SIZE_HA', 'RS_HA', 'ratio')])
    plot_data$sdSE <- (rfSE_pred$predictions / 100) * mean(plot_data$AGB_T_HA, na.rm = TRUE)
  }

  # 3. Calculate or preserve growth uncertainty
  if ("sdGrowth" %in% names(plot_data)) {
    message("Using existing growth uncertainty (sdGrowth) values")
  } else {
    if (biome_info && "GEZ" %in% names(plot_data)) {
      message("Calculating growth uncertainty by biome")
      # Get unique biomes
      gez_values <- sort(as.vector(unique(plot_data$GEZ)))

      # Apply TempVar by biome
      plot_data_list <- lapply(gez_values, function(gez) {
        gez_data <- plot_data[plot_data$GEZ == gez, ]
        if (nrow(gez_data) > 0) {
          return(TempVar(gez_data, map_year, gez = gez))
        } else {
          return(NULL)
        }
      })

      # Combine results
      plot_data <- do.call(rbind, plot_data_list)

      # Handle missing values
      plot_data$sdGrowth <- ifelse(is.na(plot_data$sdGrowth) | is.nan(plot_data$sdGrowth),
                                  mean(plot_data$sdGrowth, na.rm = TRUE),
                                  plot_data$sdGrowth)
    } else {
      message("No biome information available, using default growth uncertainty")
      plot_data$sdGrowth <- 0.05 * plot_data$AGB_T_HA
    }
  }

  # 4. Calculate total uncertainty
  plot_data$varPlot <- plot_data$sdTree^2 + plot_data$sdSE^2 + plot_data$sdGrowth^2
  plot_data$sdTotal <- sqrt(plot_data$varPlot)

  message(paste0("Total uncertainty calculated for plot data of type: ", plot_type))

  return(list(
    data = plot_data,
    plot_type = plot_type,
    uncertainty_components = c(
      measurement = mean(plot_data$sdTree^2 / plot_data$varPlot, na.rm = TRUE),
      sampling = mean(plot_data$sdSE^2 / plot_data$varPlot, na.rm = TRUE),
      growth = mean(plot_data$sdGrowth^2 / plot_data$varPlot, na.rm = TRUE)
    )
  ))
}

#' Determine plot data type from structure and columns
#'
#' This helper function identifies the type of plot data based on its structure and available columns.
#' It performs a series of checks in the following priority order:
#'
#' 1. Checks if data is an sf object (classifies as "polygon")
#' 2. Checks for column names indicating nested plot structure
#' 3. Checks for complex IDs or GUIDs in ID columns that suggest nested structure
#' 4. Checks for processed tree-level data with AGB and standard deviation
#' 5. Checks for raw tree data with standard column names
#' 6. Checks for non-standard tree data based on column name patterns
#' 7. Checks for LiDAR indicators like "cv" or "raster" columns
#' 8. Defaults to "point" type if no other type is detected
#'
#' @param plot_data A data frame containing plot data
#' @return Character string indicating the plot data type: "tree_level", "point", "polygon", "nested", or "lidar"
#' @keywords internal
determineDataType <- function(plot_data) {
  # Check if it's an sf object indicating polygon data
  if (inherits(plot_data, "sf")) {
    return("polygon")
  }

  # Check for columns that clearly indicate nested structure (priority 1)
  if (any(grepl("subplot|sub_plot|nested|nest|point_guid|tree_guid|subpoint", names(plot_data), ignore.case = TRUE))) {
    return("nested")
  }

  # Check for complex/GUID IDs which strongly indicate nested structure (priority 2)
  id_columns <- intersect(c("PLOT_ID", "id", "plot_id", "plotId", "POINT_GUID"), names(plot_data))
  if (length(id_columns) > 0) {
    col_name <- id_columns[1]
    if (is.character(plot_data[[col_name]]) && length(plot_data[[col_name]]) > 0 &&
        (any(nchar(as.character(plot_data[[col_name]])) > 20) ||
         any(grepl("[-{}]", as.character(plot_data[[col_name]]))))) {
      return("nested")
    }
  }

  # Check for columns that would indicate tree-level data that has been processed (priority 3)
  if (all(c("sdTree", "AGB_T_HA") %in% names(plot_data))) {
    # Check for LiDAR indicators
    if (any(grepl("lidar|cv|raster", names(plot_data), ignore.case = TRUE)) ||
        ("CV" %in% names(plot_data))) {
      return("lidar")
    }

    return("tree_level")
  }

  # Check for raw tree data columns (priority 4)
  if (all(c("diameter", "genus", "species") %in% names(plot_data)) ||
      all(c("DBH", "genus", "species") %in% names(plot_data)) ||  # Alternative column name
      (("diameter" %in% names(plot_data) || "DBH" %in% names(plot_data)) &&
       "height" %in% names(plot_data))) {
    return("tree_level")
  }

  # Check for columns that indicate tree-level data without standard column names (priority 5)
  treeLevelIndicators <- c("dbh", "diam", "diameter", "tree", "stem", "species", "genus", "height", "allom")
  if (sum(sapply(names(plot_data), function(col) any(grepl(paste(treeLevelIndicators, collapse="|"), col, ignore.case=TRUE)))) >= 2) {
    return("tree_level")
  }

  # Check for possible LiDAR indicators - more comprehensive check (priority 6)
  if (any(grepl("lidar|cv|raster|intensi|return|pulse|scan|point_cloud|las|laz",
                names(plot_data), ignore.case = TRUE)) ||
      ("CV" %in% names(plot_data))) {
    return("lidar")
  }

  # Default to point if nothing else matches
  return("point")
}


