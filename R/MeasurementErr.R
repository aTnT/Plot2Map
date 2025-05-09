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
# 06/05/2025:
# Improved NA handling and error detection in calculateTotalUncertainty:
# 1. Added explicit NA checks before varPlot calculation with appropriate warnings
# 2. Implemented improved RF model loading with better error handling and environment isolation
# 3. Enhanced sampling error prediction with input validation and error handling
# 4. Added more informative error messages throughout the uncertainty calculation process
# 5. Added se_data parameter to allow using custom sampling error data instead of loading from file
# 6. Enhanced function documentation with detailed explanations of uncertainty components and calculations
# 7. Fixed and expanded examples with comprehensive use cases for all function parameters
# 8. Added detailed parameter descriptions with required columns and expected formats
# 9. Added comprehensive documentation of the step-by-step calculation process


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
#' @importFrom dplyr filter select group_by summarize mutate inner_join arrange
#' @importFrom BIOMASS correctTaxo getWoodDensity modelHD retrieveH AGBmonteCarlo
#'
#' @examples
#' plotsTree <- utils::read.csv(sample_file("SampleTree.csv"))
#' head(plotsTree)
#'
#' xyTree <- utils::read.csv(sample_file("SampleTreeXY.csv"))
#' head(xyTree)
#'
#' plot_uncertainties <- sd_tree(plotsTree, xyTree, region = "India")
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
#' The function handles three primary sources of uncertainty:
#' \itemize{
#'   \item \strong{Measurement uncertainty (sdTree)}: Derived from allometric equations, wood density estimates,
#'   and measurement error. For tree-level data, this is calculated by the BIOMASS package with Monte Carlo methods.
#'   For plot-level data, it's estimated using a random forest model trained on a large dataset.
#'   \item \strong{Sampling uncertainty (sdSE)}: Accounts for the sampling error introduced by differing
#'   plot sizes vs. pixel sizes, following the approach from Réjou-Méchain et al. (2014).
#'   \item \strong{Growth uncertainty (sdGrowth)}: Adjusts for temporal differences between plot measurement year
#'   and map production year, using growth rates specific to each ecological zone.
#' }
#'
#' These components are combined using error propagation principles:
#' \deqn{varPlot = sdTree^2 + sdSE^2 + sdGrowth^2}
#' \deqn{sdTotal = \sqrt{varPlot}}
#'
#' @section Calculation Process:
#'
#' \subsection{Step 1: Measurement Error (sdTree)}{
#' \itemize{
#'   \item \strong{For tree-level data}: Uses BIOMASS package Monte Carlo simulations that account for wood density
#'   uncertainty, diameter measurement errors, and allometric model errors.
#'   \item \strong{For plot-level data}: Uses a random forest model trained on thousands of plots across biomes to
#'   predict measurement uncertainty based on plot AGB, plot size, and ecological zone.
#'   \item If a pre-existing sdTree column exists, its values are preserved.
#' }
#' }
#'
#' \subsection{Step 2: Sampling Error (sdSE)}{
#' \itemize{
#'   \item Uses a random forest model trained on geo-simulation data that relates plot-to-pixel size ratios to
#'   coefficient of variation.
#'   \item The model considers the relationship between plot size (SIZE_HA), remote sensing pixel size (RS_HA),
#'   and their ratio.
#'   \item The resulting coefficient of variation is converted to standard deviation by multiplying by mean AGB.
#'   \item Custom sampling error data can be provided through the \code{se_data} parameter.
#' }
#' }
#'
#' \subsection{Step 3: Growth Uncertainty (sdGrowth)}{
#' \itemize{
#'   \item If \code{biome_info = TRUE}, uses biome-specific growth rates and uncertainty from the TempVar function.
#'   \item The function considers the time difference between plot measurement year (AVG_YEAR) and map year.
#'   \item Each ecological zone (GEZ) is processed separately to apply biome-specific growth rates.
#'   \item If biome information isn't available or \code{biome_info = FALSE}, the function requires that \code{sdGrowth} column be provided in the input data.
#' }
#' }
#'
#' \subsection{Step 4: Total Uncertainty}{
#' \itemize{
#'   \item Combines all uncertainty components by summing their variances: \code{varPlot = sdTree^2 + sdSE^2 + sdGrowth^2}
#'   \item Calculates total standard deviation as \code{sdTotal = sqrt(varPlot)}
#'   \item Reports the relative contribution of each component to the total variance.
#' }
#' }
#'
#' @param plot_data A data frame containing plot data. Required columns:
#'   \itemize{
#'     \item \code{PLOT_ID} - Plot identifier
#'     \item \code{AGB_T_HA} - Above-ground biomass in tons per hectare
#'     \item \code{SIZE_HA} - Plot size in hectares
#'     \item \code{GEZ} - Global ecological zone (e.g., "Tropical", "Temperate", "Boreal", "Subtropical")
#'   }
#'
#'   Additional columns required when \code{biome_info=TRUE}:
#'   \itemize{
#'     \item \code{ZONE} - Continental zone (e.g., "Europe", "Asia", "Africa")
#'     \item \code{AVG_YEAR} - Year of plot measurement for temporal adjustment
#'   }
#'
#'   If \code{sdTree}, \code{sdSE}, or \code{sdGrowth} columns exist, they will be preserved.
#'
#' @param map_year Numeric value indicating the map year for temporal adjustment. This is used
#'   to calculate growth uncertainty based on the time difference between plot measurement and map production.
#'
#' @param map_resolution Numeric value indicating the map resolution in meters (default: 100).
#'   This affects sampling error calculations based on the difference between plot size and pixel size.
#'
#' @param biome_info Logical indicating whether to use biome information (GEZ column) for growth uncertainty
#'   calculation (default: TRUE). When TRUE, the function uses TempVar which requires 'ZONE', 'GEZ', and 'AVG_YEAR'
#'   columns to be present in the plot_data. If FALSE, the function requires that \code{sdGrowth} column
#'   be provided in the input data.
#'
#' @param se_data Optional data frame with sampling error data that can be used instead of loading se.csv.
#'   Must contain columns:
#'   \itemize{
#'     \item \code{SIZE_HA} - Plot size in hectares
#'     \item \code{RS_HA} - Pixel resolution in hectares
#'     \item \code{ratio} - Ratio of plot size to pixel size
#'     \item \code{cv} - Coefficient of variation (percent)
#'   }
#'
#' @return A list containing:
#'   \describe{
#'     \item{data}{The input plot data with additional uncertainty columns:
#'       \itemize{
#'         \item \code{sdTree} - Standard deviation from measurement error
#'         \item \code{sdSE} - Standard deviation from sampling error
#'         \item \code{sdGrowth} - Standard deviation from growth/temporal uncertainty
#'         \item \code{varPlot} - Total variance (sum of squared standard deviations)
#'         \item \code{sdTotal} - Total standard deviation (square root of varPlot)
#'       }
#'     }
#'     \item{plot_type}{The detected plot data type: "tree_level", "point", "polygon", "nested", or "lidar"}
#'     \item{uncertainty_components}{The relative contribution of each uncertainty component as a proportion of the total variance}
#'   }
#' @export
#' @examples
#' # Example 1: Using existing plot data
#' # First load the sample plots dataset included with the package
#' data(plots)
#'
#' # Create a subset of plots for demonstration
#' set.seed(42)
#' sample_plots <- plots[sample(nrow(plots), 5), ]
#'
#' # Add biome information (required for uncertainty calculation)
#' sample_plots <- BiomePair(sample_plots)
#'
#' # Apply TempApplyVar to adjust biomass to map year and calculate temporal variance
#' sample_plots <- TempApplyVar(sample_plots, map_year = 2020)
#'
#' # Calculate total uncertainty components for map year 2020
#' result <- calculateTotalUncertainty(sample_plots, map_year = 2020, map_resolution = 100)
#'
#' # View the results
#' head(result$data)
#'
#' # See the relative contribution of each uncertainty component
#' result$uncertainty_components
#'
#' # Example 2: Working with data that already has measurement uncertainty
#' # First process tree-level data to get measurement uncertainty
#' # Load sample tree data
#' tree_data <- utils::read.csv(sample_file("SampleTree.csv"))
#' tree_locations <- utils::read.csv(sample_file("SampleTreeXY.csv"))
#'
#' # Calculate AGB and measurement uncertainty
#' tree_results <- sd_tree(tree_data, tree_locations, region = "World")
#'
#' # Add required biome information
#' tree_results <- BiomePair(tree_results)
#'
#' # Calculate total uncertainty with tree data
#' tree_uncertainty <- calculateTotalUncertainty(tree_results, map_year = 2020)
#'
#' # Example 3: Using custom sampling error data
#' # Create custom sampling error data (coefficient of variation by plot/pixel size ratio)
#' custom_se_data <- data.frame(
#'   SIZE_HA = c(0.1, 0.25, 0.5, 1.0),  # Plot sizes in hectares
#'   RS_HA = c(1, 1, 1, 1),            # Remote sensing pixel size in hectares
#'   ratio = c(0.1, 0.25, 0.5, 1.0),   # Ratio of plot size to pixel size
#'   cv = c(20, 15, 10, 5)             # Coefficient of variation (%)
#' )
#'
#' # Calculate uncertainty using custom sampling error model
#' custom_result <- calculateTotalUncertainty(
#'   sample_plots,
#'   map_year = 2020,
#'   map_resolution = 100,
#'   se_data = custom_se_data
#' )
#'
#' # Example 4: Using manual growth uncertainty values
#' # First, prepare data with manual sdGrowth values
#' manual_growth_plots <- sample_plots
#' # Add manual growth uncertainty of 7% of AGB
#' manual_growth_plots$sdGrowth <- manual_growth_plots$AGB_T_HA * 0.07
#'
#' # Now calculate uncertainty with biome_info = FALSE
#' simple_result <- calculateTotalUncertainty(
#'   manual_growth_plots,
#'   map_year = 2020,
#'   map_resolution = 100,
#'   biome_info = FALSE
#' )
calculateTotalUncertainty <- function(plot_data, map_year, map_resolution = 100, biome_info = TRUE, se_data = NULL) {

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

      # Check for ranger package availability first
      if (!requireNamespace("ranger", quietly = TRUE)) {
        stop("The ranger package is required but not installed. Please install it with: install.packages('ranger')")
      }
      # Load RF model from package data
      rf1_path <- sample_file("rf1.RData")
      if (!file.exists(rf1_path)) {
        stop("RF model file 'rf1.RData' not found in package data. Please ensure the package is properly installed.")
      }

      # Try to use existing model or load from file
      rf_model_env <- new.env()

      tryCatch({
        # Load model into a separate environment to avoid global environment conflicts
        load(rf1_path, envir = rf_model_env)

        if (!exists("rf1", envir = rf_model_env)) {
          stop("The loaded RData file does not contain an 'rf1' model object")
        }

        # Verify the loaded object is a ranger model
        if (!inherits(rf_model_env$rf1, "ranger")) {
          stop("The loaded 'rf1' object is not a ranger model")
        }

        # Make prediction with appropriate error handling
        predictions <- predict(rf_model_env$rf1, data = plotsPred)

        # Extract predictions with validation
        if (!is.null(predictions) && !is.null(predictions$predictions)) {
          plot_data$sdTree <- predictions$predictions
        } else {
          stop("RF model prediction failed to produce valid predictions")
        }
      }, error = function(e) {
        stop(paste("Error in RF model processing:", e$message))
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
    rfSE <- NULL
    # First check if ranger is available
    if (!requireNamespace("ranger", quietly = TRUE)) {
      stop("The ranger package is required but not installed. Please install it with: install.packages('ranger')")
    }

    # Check for existing model in global environment
    if ("rfSE" %in% ls(envir = .GlobalEnv)) {
      rfSE <- get("rfSE", envir = .GlobalEnv)

      # Verify that the model is a valid ranger object
      if (!inherits(rfSE, "ranger")) {
        warning("The global rfSE object is not a valid ranger model. Creating a new one.")
        rfSE <- NULL
      }
    }
    # If no valid model exists, create a new one from the data
    if (is.null(rfSE)) {
      # Use provided se_data if available, otherwise load from file
      se <- NULL
      if (!is.null(se_data)) {
        # Use provided sampling error data
        message("Using provided sampling error data")
        se <- se_data
      } else {
        # Load se.csv from package data
        message("Loading sampling error data from package file")
        se_file <- sample_file("se.csv")
        if (!file.exists(se_file)) {
          stop("Sampling error data file 'se.csv' not found in package data. Please ensure the package is properly installed.")
        }
        # Load data from file
        tryCatch({
          se <- read.csv(se_file)
        }, error = function(e) {
          stop(paste("Error reading sampling error data file:", e$message))
        })
      }
      # Verify and use the data
      tryCatch({
        # Verify the data has the required columns
        required_cols <- c('SIZE_HA', 'RS_HA', 'ratio', 'cv')
        if (is.null(se) || !all(required_cols %in% names(se))) {
          stop(paste("Sampling error data doesn't contain all required columns:",
                    paste(required_cols, collapse = ", ")))
        }

        # Train the ranger model
        rfSE <- ranger::ranger(se$cv ~ ., data = se[, c('SIZE_HA', 'RS_HA', 'ratio')])

        # Verify model was created correctly
        if (!inherits(rfSE, "ranger")) {
          stop("Failed to create a valid ranger model for sampling error")
        }
      }, error = function(e) {
        stop(paste("Error creating sampling error model:", e$message))
      })
    }

    # Predict sampling error using random forest model
    # Convert to standard deviation by multiplying by mean AGB
    tryCatch({
      # Ensure we have valid data for prediction
      pred_data <- plot_data[, c('SIZE_HA', 'RS_HA', 'ratio')]
      # Check for NA or invalid values in prediction data
      if (any(is.na(pred_data))) {
        warning("NA values found in size ratio data. These will produce NA predictions.")
      }

      # Make prediction
      rfSE_pred <- predict(rfSE, data = pred_data)

      if (is.null(rfSE_pred) || is.null(rfSE_pred$predictions)) {
        stop("Sampling error model prediction failed to produce valid results")
      }

      # Calculate mean AGB for scaling
      mean_agb <- mean(plot_data$AGB_T_HA, na.rm = TRUE)
      if (is.na(mean_agb)) {
        warning("All AGB values are NA. Cannot calculate sdSE.")
      }

      # Calculate standard deviation from coefficient of variation
      plot_data$sdSE <- (rfSE_pred$predictions / 100) * mean_agb

    }, error = function(e) {
      stop(paste("Error in sampling error prediction:", e$message))
    })
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
      stop("Growth uncertainty (sdGrowth) must be provided in the input data when biome_info=FALSE. ",
           "Please add a 'sdGrowth' column to your plot_data before calling this function.")
    }
  }

  # 4. Calculate total uncertainty with careful NA handling
  # Check for NA values in any component before calculating varPlot
  has_na_components <- is.na(plot_data$sdTree) | is.na(plot_data$sdSE) | is.na(plot_data$sdGrowth)
  if (any(has_na_components)) {
    warning(paste("Found", sum(has_na_components), "rows with NA in uncertainty components.",
                 "These will result in NA varPlot values."))
  }
  # Calculate variance and total standard deviation
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


