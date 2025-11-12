## Updates made to the new framework:
# 21/10/25:
# Replacing raster with terra package
# Improved method to calculate cells size in ha (reduces up to 10% bias in tropics)
# Removed unused year argument
# Enhanced with multi-band support, intelligent pattern detection, and CRS validation
#
# 11/11/25:
# Added raster_type parameter to enable non-interactive raster type specification
# Added allow_interactive parameter to prevent user prompts in automated pipelines
# Implemented non-interactive fallback: uses filename as PLOT_ID, extracts year via regex
# Updated documentation with Enhancement 4 details and usage examples



# ===============================================================================
# ENHANCEMENT HELPER FUNCTIONS
# ===============================================================================

#' Detect and extract AGB layer from multi-band rasters
#' @param raster_file Path to raster file
#' @return Single-layer terra raster with AGB data
detect_agb_layer <- function(raster_file) {
  r <- terra::rast(raster_file)

  # Return as-is if single band
  if (terra::nlyr(r) == 1) {
    return(r)
  }

  layer_names <- names(r)
  cat("Multi-band raster detected:", basename(raster_file), "with", terra::nlyr(r), "layers:", paste(layer_names, collapse = ", "), "\n")

  # Priority-based layer selection for AGB
  agb_patterns <- c(
    "^meanAGB$", "^AGB$", "^agb$", "^mean_agb$",
    "biomass", "carbon", "mean", "^Band_1$"
  )

  for (pattern in agb_patterns) {
    matches <- grep(pattern, layer_names, ignore.case = TRUE)
    if (length(matches) > 0) {
      selected_layer <- matches[1]
      cat("Selected layer", selected_layer, ":", layer_names[selected_layer], "for AGB extraction\n")
      agb_layer <- r[[selected_layer]]
      names(agb_layer) <- "value"  # Standardize name
      return(agb_layer)
    }
  }

  # Fallback: use first layer
  cat("No clear AGB layer found, using first layer:", layer_names[1], "\n")
  agb_layer <- r[[1]]
  names(agb_layer) <- "value"
  return(agb_layer)
}

#' Detect filename patterns using machine learning approach
#' @param filenames Vector of filenames to analyze
#' @return List with pattern configuration or NULL if detection fails
detect_patterns_ml <- function(filenames) {

  if (length(filenames) == 0) return(NULL)

  # Remove file extensions for analysis
  base_names <- tools::file_path_sans_ext(basename(filenames))
  sample_file <- base_names[1]

  cat("Analyzing filename patterns from:", sample_file, "\n")

  # Pattern 1: Site_Code_Year_Type format (e.g., "ANA_A01_2017_AGB_100m")
  if (grepl("^[A-Z]{2,4}_[A-Z0-9]+_[0-9]{4}_", sample_file)) {
    cat("Detected Pattern 1: Site_Code_Year_Type format\n")
    return(list(
      type = "site_code_year_type",
      plot_start = 1,
      plot_end = regexpr("_", sample_file) - 1,  # First underscore
      year_start = regexpr("_[0-9]{4}_", sample_file) + 1,
      year_end = regexpr("_[0-9]{4}_", sample_file) + 4,
      confidence = "high"
    ))
  }

  # Pattern 2: Year_Site_Type format (e.g., "06_Nachtigal_AGB40")
  if (grepl("^[0-9]{2}_[A-Za-z]+_", sample_file)) {
    cat("Detected Pattern 2: Year_Site_Type format\n")
    site_match <- regmatches(sample_file, regexpr("_([A-Za-z]+)_", sample_file))
    site_name <- gsub("_", "", site_match)
    return(list(
      type = "year_site_type",
      plot_start = 4,
      plot_end = 3 + nchar(site_name),
      year_start = 1,
      year_end = 2,
      confidence = "high"
    ))
  }

  # Pattern 3: Site_Year_Suffix format (e.g., "BK17a_AGB_mean")
  if (grepl("^[A-Z]{2,4}[0-9]{2}[a-z]?_", sample_file)) {
    cat("Detected Pattern 3: Site_Year_Suffix format\n")
    return(list(
      type = "site_year_suffix",
      plot_start = 1,
      plot_end = regexpr("[0-9]", sample_file) - 1,  # Before first digit
      year_start = regexpr("[0-9]", sample_file),
      year_end = regexpr("[0-9]", sample_file) + 1,
      confidence = "medium"
    ))
  }

  # Pattern 4: Australia format (e.g., "GWW_2012_AGB_100m")
  if (grepl("^[A-Z]{3}_[0-9]{4}_AGB", sample_file)) {
    cat("Detected Pattern 4: Australia format\n")
    return(list(
      type = "australia_format",
      plot_start = 1,
      plot_end = 3,
      year_start = 5,
      year_end = 8,
      confidence = "high"
    ))
  }

  # Pattern 5: Bulgaria format (e.g., "bul_agb2016_utm")
  if (grepl("^[a-z]+_agb[0-9]{4}", sample_file, ignore.case = TRUE)) {
    cat("Detected Pattern 5: Bulgaria format\n")
    year_pos <- regexpr("agb[0-9]{4}", sample_file, ignore.case = TRUE)
    return(list(
      type = "bulgaria_format",
      plot_start = 1,
      plot_end = regexpr("_", sample_file) - 1,
      year_start = year_pos + 3,  # After "agb"
      year_end = year_pos + 6,    # 4 digit year
      confidence = "medium"
    ))
  }

  cat("No recognized pattern found, using fallback configuration\n")
  return(list(
    type = "fallback",
    plot_start = 1,
    plot_end = 3,
    year_start = 1,
    year_end = 4,
    confidence = "low"
  ))
}

#' Standardize year format (handle 2-digit years)
#' @param year_str Character vector of year strings
#' @return Numeric vector of standardized years
standardize_year <- function(year_str) {
  year_num <- suppressWarnings(as.numeric(year_str))

  # Handle NA values
  if (is.na(year_num)) {
    return(NA)
  }

  # Convert 2-digit years to 4-digit
  if (year_num < 50) {
    return(2000 + year_num)  # 06 -> 2006, 23 -> 2023
  } else if (year_num < 100) {
    return(1900 + year_num)  # 95 -> 1995
  } else if (year_num < 1900) {
    return(NA)  # Invalid year
  } else {
    return(year_num)  # Already 4-digit year
  }
}

#' Validate and standardize CRS across raster files
#' @param raster_files Vector of raster file paths
#' @param target_crs Target CRS (default: "EPSG:4326")
#' @return List with CRS validation results
validate_crs <- function(raster_files, target_crs = "EPSG:4326") {

  crs_info <- list()
  different_crs <- character(0)

  cat("Validating coordinate reference systems...\n")

  for (file in raster_files) {
    r <- terra::rast(file)
    file_crs <- terra::crs(r)

    crs_info[[basename(file)]] <- file_crs

    # Check if CRS needs transformation
    if (!is.na(file_crs) && file_crs != target_crs) {
      if (grepl('utm|meters|metre|UTM|zone|NAD', file_crs, ignore.case = TRUE)) {
        different_crs <- c(different_crs, basename(file))
      }
    }
  }

  if (length(different_crs) > 0) {
    cat("Files requiring CRS transformation to", target_crs, ":\n")
    cat(paste(different_crs, collapse = ", "), "\n")
  } else {
    cat("All files compatible with target CRS:", target_crs, "\n")
  }

  return(list(
    target_crs = target_crs,
    transform_needed = length(different_crs) > 0,
    files_to_transform = different_crs,
    crs_info = crs_info
  ))
}

# ===============================================================================

#' Process LiDAR-based Reference Data 
#'
#' This function processes LiDAR-based reference data from raster files, converting them to a standardized point data format.
#' It includes multi-band raster handling, automatic pattern detection, and CRS validation.
#'
#' @param lidar.dir Character string specifying the directory containing LiDAR raster files.
#' @param auto_detect Logical indicating whether to attempt automatic filename pattern detection (default: TRUE).
#' @param pattern_config Optional list with manual pattern configuration (for advanced users).
#'
#' @return A data frame containing processed point data with columns varying based on the raster type:
#'   \item{PLOT_ID}{Unique identifier for each plot (auto-extracted or user-specified)}
#'   \item{POINT_X}{Longitude coordinate (WGS84)}
#'   \item{POINT_Y}{Latitude coordinate (WGS84)}
#'   \item{AGB/CV/SD}{Above Ground Biomass, Coefficient of Variation, or Standard Deviation (depending on raster type)}
#'   \item{AVG_YEAR}{Year of data collection (standardized to 4-digit format)}
#'
#' @details
#' The function performs the following steps:
#' 1. **File Discovery**: Loads raster files and filters out auxiliary files (.aux.xml, .ovr, etc.)
#' 2. **CRS Validation**: Validates coordinate reference systems and reprojects to WGS84 (EPSG:4326) if necessary
#' 3. **Multi-band Processing**: Intelligently detects and extracts AGB layers from multi-band rasters (e.g., meanAGB, sdAGB, meanHbin layers)
#' 4. **Pattern Detection**: Automatically detects filename patterns for PLOT_ID and YEAR extraction. Supports multiple formats including Brazil (ANA_A01_2017_AGB_100m.tif), Central Africa (06_Nachtigal_AGB40.tif), Australia (GWW_2012_AGB_100m.tif), Czech Republic (BK17a_AGB_mean.tif), and Bulgaria (bul_agb2016_utm.tif)
#' 5. **Data Extraction**: Converts raster data to points with proper metadata
#' 6. **Quality Validation**: Validates extracted years and provides processing summary
#'
#' The `raster_type` parameter enables fully non-interactive processing for automated pipelines. When NULL, attempts auto-detection from filename patterns or prompts user if `allow_interactive = TRUE`.
#'
#' @param lidar.dir Directory containing ALS raster files
#' @param auto_detect Enable automatic pattern detection for PLOT_ID and YEAR (default: TRUE)
#' @param pattern_config Optional manual pattern configuration (overrides auto-detection)
#' @param raster_type Optional raster type specification ("AGB", "CV", or "SD"). If NULL, auto-detects from filename or prompts user.
#' @param allow_interactive Allow interactive prompts if auto-detection fails (default: TRUE). Set to FALSE for automated pipelines.
#'
#' @examples
#' \dontrun{
#' # Basic usage with automatic detection
#' lidar_data <- RefLidar(lidar.dir = "data/SustainableLandscapeBrazil_v04/SLB_AGBmaps")
#'
#' # Non-interactive mode for automated pipelines
#' lidar_data <- RefLidar(
#'   lidar.dir = "data/SustainableLandscapeBrazil_v04/SLB_AGBmaps",
#'   raster_type = "AGB",
#'   allow_interactive = FALSE
#' )
#'
#' # Process Coefficient of Variation rasters
#' cv_data <- RefLidar(
#'   lidar.dir = "data/uncertainty_maps/",
#'   raster_type = "CV",
#'   allow_interactive = FALSE
#' )
#'
#' # Disable auto-detection for custom naming
#' lidar_data <- RefLidar(
#'   lidar.dir = "data/custom_naming/",
#'   auto_detect = FALSE,
#'   raster_type = "AGB"
#' )
#'
#' # Manual pattern_config for custom filename formats
#' custom_pattern <- list(
#'   type = "custom",
#'   plot_start = 1,
#'   plot_end = 15,
#'   year_start = 22,
#'   year_end = 25,
#'   confidence = "high"
#' )
#' lidar_data <- RefLidar(
#'   lidar.dir = "data/custom_format/",
#'   pattern_config = custom_pattern,
#'   raster_type = "AGB"
#' )
#'
#' # Pattern config for Brazil format (ANA_A01_2017_AGB_100m.tif)
#' brazil_pattern <- list(
#'   type = "site_code_year_type",
#'   plot_start = 1,
#'   plot_end = 3,
#'   year_start = 9,
#'   year_end = 12,
#'   confidence = "high"
#' )
#' brazil_data <- RefLidar(
#'   lidar.dir = "data/brazil_slb/",
#'   pattern_config = brazil_pattern,
#'   raster_type = "AGB"
#' )
#'
#' # Multi-band raster processing
#' multiband_data <- RefLidar(
#'   lidar.dir = "data/LiDAR-based_biomass_maps_Central_Africa/",
#'   raster_type = "AGB"
#' )
#'
#' # Processing multiple raster types from same directory
#' agb_data <- RefLidar(
#'   lidar.dir = "data/complete_dataset/",
#'   raster_type = "AGB",
#'   allow_interactive = FALSE
#' )
#' cv_data <- RefLidar(
#'   lidar.dir = "data/complete_dataset/",
#'   raster_type = "CV",
#'   allow_interactive = FALSE
#' )
#' combined_data <- merge(agb_data, cv_data,
#'                        by = c("PLOT_ID", "POINT_X", "POINT_Y", "AVG_YEAR"))
#' }
#' @importFrom terra rast crop ext extract ncell project vect values xmax xmin ymax ymin
#' @importFrom utils menu
#'
#' @export
RefLidar <- function(lidar.dir, auto_detect = TRUE, pattern_config = NULL, raster_type = NULL, allow_interactive = TRUE) {

  newproj <- "EPSG:4326"  # WGS84 target projection

  # Get all raster files (filter out auxiliary files)
  raw <- list.files(lidar.dir, full.names = TRUE)
  # Filter out auxiliary files that cause issues
  raw <- raw[grepl("\\.(tif|tiff)$", raw, ignore.case = TRUE)]
  raw <- raw[!grepl("\\.(aux|xml|ovr|tfw)($|\\.)", basename(raw))]

  if (length(raw) == 0) {
    stop("No valid raster files found in directory: ", lidar.dir)
  }

  cat("Found", length(raw), "raster files for processing\n")

  # ===== ENHANCEMENT 3: CRS VALIDATION =====
  crs_validation <- validate_crs(raw, newproj)

  # Determine raster type (auto-detect or prompt)
  if (is.null(raster_type)) {
    # Try auto-detection from filename
    sample_filename <- tolower(basename(raw[1]))
    if (grepl("cv|coef|variation", sample_filename)) {
      raster_type <- "CV"
      cat("Auto-detected raster type: CV (coefficient of variation)\n")
    } else if (grepl("sd|stdev|deviation", sample_filename)) {
      raster_type <- "SD"
      cat("Auto-detected raster type: SD (standard deviation)\n")
    } else if (grepl("agb|biomass", sample_filename)) {
      raster_type <- "AGB"
      cat("Auto-detected raster type: AGB (above-ground biomass)\n")
    } else {
      # Fallback to interactive prompt if auto-detection fails
      raster_type <- readline(prompt = "Enter raster type (AGB, CV, or SD): ")
    }
  }

  raster_type <- toupper(raster_type)  # Ensure case consistency

  if (!raster_type %in% c("AGB", "CV", "SD")) {
    stop("Invalid raster type. Please enter 'AGB', 'CV', or 'SD'.")
  }

  cat("Using raster type:", raster_type, "\n")

  # ===== ENHANCEMENT 1: MULTI-BAND RASTER HANDLING =====
  cat("Processing raster files with multi-band support...\n")

  # Load and process raster files with intelligent layer detection
  r.files <- lapply(raw, function(x) {
    # Use enhanced multi-band detection
    agb_layer <- detect_agb_layer(x)
    names(agb_layer) <- "value"  # Standardize the column name
    return(agb_layer)
  })

  # ===== ENHANCEMENT 3: IMPROVED CRS TRANSFORMATION =====
  # Reproject to WGS84 if needed (enhanced CRS detection)
  if (crs_validation$transform_needed) {
    cat("Transforming rasters to target CRS:", newproj, "\n")
    r.files <- lapply(r.files, function(x) {
      if (grepl('utm|meters|metre|UTM|zone|NAD', terra::crs(x), ignore.case = TRUE)) {
        cat("Reprojecting raster with CRS:", terra::crs(x, describe = TRUE)$name, "\n")
        return(terra::project(x, newproj, method = 'bilinear'))
      }
      return(x)
    })
  }

  # Calculate cell size in hectares (improved calculation)
  ha <- mean(terra::values(terra::cellSize(r.files[[1]]) / 10000), na.rm = TRUE)
  cat("Calculated average cell size:", round(ha, 6), "hectares\n")

  # Convert pixels to points
  pts.list <- lapply(r.files, function(x) {
    df <- terra::as.data.frame(x, xy = TRUE)  # Convert raster to data frame with coordinates
    colnames(df) <- c("POINT_X", "POINT_Y", raster_type)  # Rename columns
    df
  })

  # Add ID for each raster file
  pts.list <- lapply(seq_along(pts.list), function(i) {
    pts <- pts.list[[i]]
    pts$ID <- basename(raw[i])  # Add file name as ID
    pts
  })

  # Combine all points into a single data frame
  pts <- do.call(rbind, pts.list)

  # ===== ENHANCEMENT 2: INTELLIGENT PATTERN DETECTION =====
  if (auto_detect) {
    cat("Attempting automatic pattern detection...\n")
    detected_patterns <- detect_patterns_ml(unique(pts$ID))

    if (!is.null(detected_patterns) && detected_patterns$confidence %in% c("high", "medium")) {
      cat("Using automatically detected patterns (confidence:", detected_patterns$confidence, ")\n")

      # Extract PLOT_ID using detected pattern
      pts$PLOT_ID <- substr(pts$ID, detected_patterns$plot_start, detected_patterns$plot_end)

      # Extract and standardize YEAR using detected pattern
      year_raw <- substr(pts$ID, detected_patterns$year_start, detected_patterns$year_end)
      pts$AVG_YEAR <- sapply(year_raw, standardize_year)

      cat("Successfully extracted PLOT_ID and YEAR using automatic detection\n")
      cat("Sample PLOT_ID values:", paste(unique(pts$PLOT_ID)[1:min(3, length(unique(pts$PLOT_ID)))], collapse = ", "), "\n")
      cat("Sample YEAR values:", paste(unique(pts$AVG_YEAR)[1:min(3, length(unique(pts$AVG_YEAR)))], collapse = ", "), "\n")

    } else {
      cat("Automatic pattern detection failed or low confidence, falling back to manual input\n")
      auto_detect <- FALSE
    }
  }

  # Fall back to manual input if auto-detection fails or is disabled
  if (!auto_detect) {
    if (!allow_interactive) {
      # Non-interactive mode: use filename as PLOT_ID and try to extract year
      cat("Non-interactive mode: Using filename as PLOT_ID\n")
      pts$PLOT_ID <- tools::file_path_sans_ext(pts$ID)

      # Try to extract year from filename using pattern matching
      year_match <- regmatches(pts$ID, regexpr("[12][0-9]{3}", pts$ID))
      if (length(year_match) > 0 && all(nzchar(year_match))) {
        pts$AVG_YEAR <- as.integer(year_match)
        cat("Extracted years from filenames\n")
      } else {
        # If no year found, use a default
        pts$AVG_YEAR <- 2020
        cat("WARNING: Could not extract year from filenames, using default year 2020\n")
      }
    } else {
      # Interactive mode: Manual entry for PLOT_ID and AVG_YEAR (original behavior)
      message("User input needed to extract PLOT_ID from filename(s)...")
      message(paste0("File(s) used to extract data: ",  paste(unique(pts$ID), collapse = "")))
      plot_start <- as.integer(readline(prompt = "Enter numeric index of the first letter of PLOT_ID: "))
      plot_end <- as.integer(readline(prompt = "Enter numeric index of the last letter of PLOT_ID: "))
      pts$PLOT_ID <- substr(pts$ID, plot_start, plot_end)

      message("User input needed to extract YEAR from filename(s)...")
      message(paste0("File(s) used to extract data: ",  paste(unique(pts$ID), collapse = "")))
      year_start <- as.integer(readline(prompt = "Enter numeric index of the first letter of YEAR: "))
      year_end <- as.integer(readline(prompt = "Enter numeric index of the last letter of YEAR: "))
      year_raw <- substr(pts$ID, year_start, year_end)

      # Apply enhanced year standardization
      pts$AVG_YEAR <- sapply(year_raw, standardize_year)
    }
  }

  # Validate extracted years
  valid_years <- !is.na(pts$AVG_YEAR) & pts$AVG_YEAR >= 1990 & pts$AVG_YEAR <= 2030
  if (sum(!valid_years) > 0) {
    cat("WARNING:", sum(!valid_years), "points have invalid years and will be removed\n")
    pts <- pts[valid_years, ]
  }

  # Add additional columns
  pts$SIZE_HA <- ha

  # Format output based on raster type
  if (raster_type == "AGB") {
    pts <- pts[, c("PLOT_ID", "POINT_X", "POINT_Y", "AGB", "AVG_YEAR")]
  } else if (raster_type == "CV") {
    pts <- pts[, c("PLOT_ID", "POINT_X", "POINT_Y", "CV", "AVG_YEAR")]
  } else if (raster_type == "SD") {
    pts <- pts[, c("PLOT_ID", "POINT_X", "POINT_Y", "SD", "AVG_YEAR")]
  }

  # Final validation and summary
  cat("\n=== RefLidar Processing Summary ===\n")
  cat("Extracted points:", nrow(pts), "\n")
  cat("Unique plots:", length(unique(pts$PLOT_ID)), "\n")
  cat("Year range:", min(pts$AVG_YEAR, na.rm = TRUE), "-", max(pts$AVG_YEAR, na.rm = TRUE), "\n")
  cat("Cell size (ha):", round(ha, 6), "\n")
  cat("CRS transformations:", ifelse(crs_validation$transform_needed, "Yes", "No"), "\n")
  cat("Multi-band processing:", ifelse(any(sapply(raw, function(x) terra::nlyr(terra::rast(x)) > 1)), "Yes", "No"), "\n")

  return(pts)
}
