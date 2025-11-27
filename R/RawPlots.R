## Updates made to the new framework:
# 20/03/25:
# Update to reflect commit b9000c1 at https://github.com/arnanaraza/Plot2Map
# 07/07/25:
# Fixed bug in RawPlots causing PLOT_ID to become NA when selecting parcel id column
# 26/01/25:
# Added auto-detection for common column names
# Added allow_interactive parameter for non-interactive environments
# Implemented fallback logic similar to RefLidar()


#' Format Plot Data
#'
#' This function formats raw plot data into a standardized structure for further processing.
#' It can automatically detect common column names or prompt for user input in interactive sessions.
#'
#' @param plots A data frame containing plot data with Latitude, Longitude coordinates.
#' @param mapYear Optional. The year of the map being used for comparison (not used in current implementation).
#' @param allow_interactive Logical. Allow interactive prompts if auto-detection fails (default: TRUE).
#'   Set to FALSE for automated pipelines. When FALSE and in non-interactive sessions, the function
#'   will attempt auto-detection and provide helpful error messages if columns cannot be identified.
#' @param column_map Optional named list explicitly specifying column names. Bypasses auto-detection.
#'   Use names: id, agb, x, y, size, year. Example:
#'   \code{column_map = list(id = "PlotCode", agb = "Biomass_MgHa", x = "Easting", y = "Northing", size = "Area_ha", year = "MeasYear")}.
#'   If id is not provided or NULL, sequential IDs will be generated.
#'
#' @return A data frame with formatted plot data, including columns for PLOT_ID, POINT_X, POINT_Y, AGB_T_HA, SIZE_HA, FEZ, GEZ, and AVG_YEAR.
#'
#' @details
#' The function attempts to auto-detect columns based on common naming patterns:
#' \itemize{
#'   \item Plot ID: PLOT_ID, PlotID, ID, plot_id, plotid, etc. (if not found, generates sequential IDs)
#'   \item AGB: AGB_T_HA, AGB, agb, biomass, etc. (required)
#'   \item Longitude: longitude, lon, long, x, POINT_X, etc. (required)
#'   \item Latitude: latitude, lat, y, POINT_Y, etc. (required)
#'   \item Size: SIZE_HA, size, area, plot_size, plotsize, etc. (required)
#'   \item Year: AVG_YEAR, year, YEAR, measurement_year, etc. (required)
#' }
#'
#' If auto-detection fails for required columns and allow_interactive is TRUE in an interactive session,
#' it prompts the user to select columns manually. In non-interactive environments with
#' allow_interactive = FALSE, it provides an error listing available columns.
#'
#' Alternatively, use \code{column_map} to explicitly specify columns, bypassing auto-detection entirely.
#'
#' @examples
#' \dontrun{
#' # Auto-detection (works in both interactive and non-interactive)
#' formatted_plots <- RawPlots(raw_plots)
#'
#' # Non-interactive mode (for scripts/pipelines)
#' formatted_plots <- RawPlots(raw_plots, allow_interactive = FALSE)
#'
#' # Explicit column mapping
#' formatted_plots <- RawPlots(raw_plots,
#'   allow_interactive = FALSE,
#'   column_map = list(
#'     id = "PlotCode",
#'     agb = "Biomass_MgHa",
#'     x = "Easting",
#'     y = "Northing",
#'     size = "Area_ha",
#'     year = "MeasYear"
#'   ))
#' }
#' @export
RawPlots <- function(plots, mapYear = NULL, allow_interactive = TRUE, column_map = NULL) {
  # Type checking
  if (!is.data.frame(plots)) {
    stop('Input file should be a data frame with XY coordinates')
  }

  # Exclude list columns (e.g., geometry in spatial data)
  valid_cols <- names(plots)[!sapply(plots, is.list)]

  # If column_map is provided, use explicit mapping and skip auto-detection
  if (!is.null(column_map)) {
    # Validate column_map structure
    if (!is.list(column_map)) {
      stop("column_map must be a named list")
    }

    # Extract column names from map
    id_mapped <- column_map$id
    agb_mapped <- column_map$agb
    x_mapped <- column_map$x
    y_mapped <- column_map$y
    size_mapped <- column_map$size
    year_mapped <- column_map$year

    # Validate required columns exist
    missing_cols <- c()
    if (!is.null(agb_mapped) && !agb_mapped %in% valid_cols) missing_cols <- c(missing_cols, paste0("agb: '", agb_mapped, "'"))
    if (!is.null(x_mapped) && !x_mapped %in% valid_cols) missing_cols <- c(missing_cols, paste0("x: '", x_mapped, "'"))
    if (!is.null(y_mapped) && !y_mapped %in% valid_cols) missing_cols <- c(missing_cols, paste0("y: '", y_mapped, "'"))
    if (!is.null(size_mapped) && !size_mapped %in% valid_cols) missing_cols <- c(missing_cols, paste0("size: '", size_mapped, "'"))
    if (!is.null(year_mapped) && !year_mapped %in% valid_cols) missing_cols <- c(missing_cols, paste0("year: '", year_mapped, "'"))

    if (length(missing_cols) > 0) {
      stop(paste0(
        "Columns specified in column_map not found in data:\n",
        paste(missing_cols, collapse = ", "),
        "\n\nAvailable columns: ", paste(valid_cols, collapse = ", ")
      ))
    }

    # Check required columns are specified
    required_missing <- c()
    if (is.null(agb_mapped)) required_missing <- c(required_missing, "agb")
    if (is.null(x_mapped)) required_missing <- c(required_missing, "x")
    if (is.null(y_mapped)) required_missing <- c(required_missing, "y")
    if (is.null(size_mapped)) required_missing <- c(required_missing, "size")
    if (is.null(year_mapped)) required_missing <- c(required_missing, "year")

    if (length(required_missing) > 0) {
      stop(paste0(
        "Required columns missing from column_map: ",
        paste(required_missing, collapse = ", "),
        "\nRequired: agb, x, y, size, year (id is optional)"
      ))
    }

    cat("\n=== Using explicit column mapping ===\n")

    # Extract data from mapped columns
    if (!is.null(id_mapped) && id_mapped %in% valid_cols) {
      id <- plots[[id_mapped]]
      cat("Plot ID:", id_mapped, "\n")
    } else {
      id <- paste0("PLOT_", seq_len(nrow(plots)))
      cat("Plot ID: Generating sequential IDs (PLOT_1, PLOT_2, ...)\n")
    }

    agb <- as.numeric(as.character(plots[[agb_mapped]]))
    x <- as.numeric(as.character(plots[[x_mapped]]))
    y <- as.numeric(as.character(plots[[y_mapped]]))
    size <- as.numeric(as.character(plots[[size_mapped]]))
    year <- as.numeric(as.character(plots[[year_mapped]]))

    cat("AGB:", agb_mapped, "\n")
    cat("Longitude:", x_mapped, "\n")
    cat("Latitude:", y_mapped, "\n")
    cat("Size:", size_mapped, "\n")
    cat("Year:", year_mapped, "\n")
    cat("===================================\n\n")

    # Convert sizes from m² to ha if size > 50 (assuming m² if large)
    size <- ifelse(!is.na(size) & size > 50, size / 10000, size)

    # Initialize unused columns
    fez <- gez <- NA

    # Create and format data frame
    plt <- data.frame(PLOT_ID  = id,
                      POINT_X  = x,
                      POINT_Y  = y,
                      AGB_T_HA = agb,
                      SIZE_HA  = size,
                      FEZ      = fez,
                      GEZ      = gez,
                      AVG_YEAR = year)

    # Convert PLOT_ID to unique identifiers using factor approach
    if (!all(is.na(plt$PLOT_ID))) {
      plt$PLOT_ID <- as.character(as.factor(plt$PLOT_ID))
    }

    # Filter rows where AGB is not NA
    plt <- subset(plt, !is.na(AGB_T_HA))
    return(plt)
  }

  # Helper function to auto-detect column by common names
  auto_detect_column <- function(patterns, col_type = "column") {
    for (pattern in patterns) {
      matches <- grep(pattern, valid_cols, ignore.case = TRUE, value = TRUE)
      if (length(matches) > 0) {
        cat("Auto-detected", col_type, ":", matches[1], "\n")
        return(matches[1])
      }
    }
    return(NULL)
  }

  # Helper function for column selection or manual entry
  select_column <- function(prompt, auto_col = NULL, col_type = "column") {
    # If auto-detection succeeded, use that column
    if (!is.null(auto_col) && auto_col %in% valid_cols) {
      col_data <- plots[[auto_col]]
      # Special handling for Plot ID column
      if (grepl("Plot ID", prompt)) {
        return(col_data)
      }
      # Convert to numeric if needed
      if (!is.numeric(col_data)) {
        col_data <- as.numeric(as.character(col_data))
      }
      return(col_data)
    }

    # Check if we can/should prompt interactively
    if (!allow_interactive || !interactive()) {
      # Non-interactive mode - provide comprehensive error
      error_msg <- paste0(
        "\n=== Column Detection Summary ===\n"
      )
      if (length(detected) > 0) {
        error_msg <- paste0(error_msg, "✓ Successfully detected: ", paste(detected, collapse = ", "), "\n")
      }
      if (length(missing) > 0) {
        error_msg <- paste0(error_msg, "✗ Could not detect: ", paste(missing, collapse = ", "), "\n")
      }
      error_msg <- paste0(
        error_msg,
        "\nAvailable columns in your data: ", paste(valid_cols, collapse = ", "), "\n\n",
        "Please ensure your data has standard column names (SIZE_HA, AVG_YEAR, etc.) ",
        "or run in interactive mode to manually select columns."
      )
      stop(error_msg)
    }

    # Interactive mode: prompt user
    choice <- menu(c("Manual entry", valid_cols), title = prompt)
    if (choice == 1) {
      manual_entry <- readline("Enter the numeric value for manual entry: ")
      return(as.numeric(manual_entry))
    } else {
      colname <- valid_cols[choice - 1]
      col_data <- plots[[colname]]
      # Special handling for Plot ID column
      if (prompt == "Which column is your unique Plot ID?") {
        # Return raw column data for Plot ID without conversion
        return(col_data)
      }
      # If the column isn't numeric, try converting via character
      if (!is.numeric(col_data)) {
        col_data <- as.numeric(as.character(col_data))
      }
      return(col_data)
    }
  }

  # Attempt auto-detection for each column
  cat("\n=== Auto-detecting columns ===\n")
  id_col <- auto_detect_column(
    c("^PLOT_ID$", "^PlotID$", "^ID$", "^plot_id$", "^plotid$", "plot.*id", "^id$"),
    "Plot ID"
  )
  agb_col <- auto_detect_column(
    c("^AGB_T_HA$", "^AGB$", "^agb$", "biomass", "aboveground"),
    "AGB"
  )
  x_col <- auto_detect_column(
    c("^longitude$", "^lon$", "^long$", "^x$", "^POINT_X$", "^X$", "point.*x"),
    "longitude"
  )
  y_col <- auto_detect_column(
    c("^latitude$", "^lat$", "^y$", "^POINT_Y$", "^Y$", "point.*y"),
    "latitude"
  )
  size_col <- auto_detect_column(
    c("^SIZE_HA$", "^size$", "^area$", "plot.*size", "plotsize", "SIZE"),
    "plot size"
  )
  year_col <- auto_detect_column(
    c("^AVG_YEAR$", "^year$", "^YEAR$", "measurement.*year", "avg.*year"),
    "year"
  )

  # Collect detection results for comprehensive error reporting
  detected <- c()
  missing <- c()
  if (!is.null(id_col)) detected <- c(detected, "Plot ID") else if (!allow_interactive || !interactive()) cat("Plot ID: Will generate sequential IDs\n")
  if (!is.null(agb_col)) detected <- c(detected, "AGB") else missing <- c(missing, "AGB")
  if (!is.null(x_col)) detected <- c(detected, "longitude") else missing <- c(missing, "longitude")
  if (!is.null(y_col)) detected <- c(detected, "latitude") else missing <- c(missing, "latitude")
  if (!is.null(size_col)) detected <- c(detected, "plot size") else missing <- c(missing, "plot size")
  if (!is.null(year_col)) detected <- c(detected, "year") else missing <- c(missing, "year")
  cat("===========================\n\n")

  # Use auto-detected columns or prompt for selection
  # Plot ID: Generate sequential IDs if not found
  if (!is.null(id_col)) {
    id <- select_column("Which column is your unique Plot ID?", id_col, "Plot ID")
  } else if (!allow_interactive || !interactive()) {
    cat("Could not auto-detect Plot ID column. Generating sequential IDs: PLOT_1, PLOT_2, ...\n")
    id <- paste0("PLOT_", seq_len(nrow(plots)))
  } else {
    id <- select_column("Which column is your unique Plot ID?", id_col, "Plot ID")
  }

  # Required columns (AGB, longitude, latitude, size, year)
  agb <- select_column("Which column is your plot AGB?", agb_col, "AGB")
  x <- select_column("Select longitude column", x_col, "longitude")
  y <- select_column("Which column is your latitude?", y_col, "latitude")
  size <- select_column("Select plot size column", size_col, "plot size")
  year <- select_column("Select year column", year_col, "year")

  # Convert sizes from m² to ha if size > 50 (assuming m² if large)
  size <- ifelse(!is.na(size) & size > 50, size / 10000, size)

  # Initialize unused columns
  fez <- gez <- NA

  # Create and format data frame

  plt <- data.frame(PLOT_ID  = id,
                    POINT_X  = x,
                    POINT_Y  = y,
                    AGB_T_HA = agb,
                    SIZE_HA  = size,
                    FEZ      = fez,
                    GEZ      = gez,
                    AVG_YEAR = year)

  # Convert PLOT_ID to unique identifiers using factor approach
  if (!all(is.na(plt$PLOT_ID))) {
    # Use the same approach as RawPlotsTree for consistency
    plt$PLOT_ID <- as.character(as.factor(plt$PLOT_ID))
  }

  # Filter rows where AGB is not NA
  plt <- subset(plt, !is.na(AGB_T_HA))
  return(plt)
}



#' Format Tree-Level Plot Data
#'
#' This function formats raw tree-level plot data into a standardized structure for further processing.
#'
#' @param plots A data frame containing tree-level plot data with Latitude, Longitude coordinates.
#' @param allow_interactive Logical. Allow interactive prompts if auto-detection fails (default: TRUE).
#'   Set to FALSE for automated pipelines.
#' @param column_map Optional named list explicitly specifying column names. Bypasses interactive prompts.
#'   Use names: id, genus, species, diameter, height (optional), x, y, size, year. Example:
#'   \code{column_map = list(id = "PlotCode", genus = "Genus", species = "Species", diameter = "DBH_cm", height = "Height_m", x = "Longitude", y = "Latitude", size = "PlotArea_ha", year = "MeasYear")}.
#'
#' @return A list containing two data frames:
#'   1. Tree-level data with columns for id, genus, species, diameter, (height), size, fez, gez, year
#'   2. Plot-level data with columns for id, x, y
#'
#' @details
#' This function currently requires interactive input or properly named columns.
#' For non-interactive use, ensure columns have standard names or use `allow_interactive = FALSE`
#' with appropriate column names.
#'
#' Alternatively, use \code{column_map} to explicitly specify columns, bypassing interactive prompts entirely.
#'
#' @examples
#' \dontrun{
#' # Interactive mode
#' tree_data <- read.csv(sample_file("SampleTreeNested.csv"))
#' formatted_tree_plots <- RawPlotsTree(tree_data)
#'
#' # Non-interactive mode (requires standard column names)
#' formatted_tree_plots <- RawPlotsTree(tree_data, allow_interactive = FALSE)
#'
#' # Explicit column mapping
#' formatted_tree_plots <- RawPlotsTree(tree_data,
#'   allow_interactive = FALSE,
#'   column_map = list(
#'     id = "PlotCode",
#'     genus = "Genus",
#'     species = "Species",
#'     diameter = "DBH_cm",
#'     height = "Height_m",
#'     x = "Longitude",
#'     y = "Latitude",
#'     size = "PlotArea_ha",
#'     year = "MeasYear"
#'   ))
#' }
#'
#' @export
RawPlotsTree <- function(plots, allow_interactive = TRUE, column_map = NULL) {
  if (!is.data.frame(plots)) {
    stop('Input file should be a data frame with XY coordinates')
  }

  # Exclude list columns from selection
  valid_cols <- names(plots)[!sapply(plots, is.list)]

  # If column_map is provided, use explicit mapping and skip interactive prompts
  if (!is.null(column_map)) {
    # Validate column_map structure
    if (!is.list(column_map)) {
      stop("column_map must be a named list")
    }

    # Extract column names from map
    id_mapped <- column_map$id
    genus_mapped <- column_map$genus
    species_mapped <- column_map$species
    diameter_mapped <- column_map$diameter
    height_mapped <- column_map$height  # Optional
    x_mapped <- column_map$x
    y_mapped <- column_map$y
    size_mapped <- column_map$size
    year_mapped <- column_map$year

    # Validate required columns exist
    missing_cols <- c()
    if (!is.null(id_mapped) && !id_mapped %in% valid_cols) missing_cols <- c(missing_cols, paste0("id: '", id_mapped, "'"))
    if (!is.null(genus_mapped) && !genus_mapped %in% valid_cols) missing_cols <- c(missing_cols, paste0("genus: '", genus_mapped, "'"))
    if (!is.null(species_mapped) && !species_mapped %in% valid_cols) missing_cols <- c(missing_cols, paste0("species: '", species_mapped, "'"))
    if (!is.null(diameter_mapped) && !diameter_mapped %in% valid_cols) missing_cols <- c(missing_cols, paste0("diameter: '", diameter_mapped, "'"))
    if (!is.null(height_mapped) && !height_mapped %in% valid_cols) missing_cols <- c(missing_cols, paste0("height: '", height_mapped, "'"))
    if (!is.null(x_mapped) && !x_mapped %in% valid_cols) missing_cols <- c(missing_cols, paste0("x: '", x_mapped, "'"))
    if (!is.null(y_mapped) && !y_mapped %in% valid_cols) missing_cols <- c(missing_cols, paste0("y: '", y_mapped, "'"))
    if (!is.null(size_mapped) && !size_mapped %in% valid_cols) missing_cols <- c(missing_cols, paste0("size: '", size_mapped, "'"))
    if (!is.null(year_mapped) && !year_mapped %in% valid_cols) missing_cols <- c(missing_cols, paste0("year: '", year_mapped, "'"))

    if (length(missing_cols) > 0) {
      stop(paste0(
        "Columns specified in column_map not found in data:\n",
        paste(missing_cols, collapse = ", "),
        "\n\nAvailable columns: ", paste(valid_cols, collapse = ", ")
      ))
    }

    # Check required columns are specified
    required_missing <- c()
    if (is.null(id_mapped)) required_missing <- c(required_missing, "id")
    if (is.null(genus_mapped)) required_missing <- c(required_missing, "genus")
    if (is.null(species_mapped)) required_missing <- c(required_missing, "species")
    if (is.null(diameter_mapped)) required_missing <- c(required_missing, "diameter")
    if (is.null(x_mapped)) required_missing <- c(required_missing, "x")
    if (is.null(y_mapped)) required_missing <- c(required_missing, "y")
    if (is.null(size_mapped)) required_missing <- c(required_missing, "size")
    if (is.null(year_mapped)) required_missing <- c(required_missing, "year")

    if (length(required_missing) > 0) {
      stop(paste0(
        "Required columns missing from column_map: ",
        paste(required_missing, collapse = ", "),
        "\nRequired: id, genus, species, diameter, x, y, size, year (height is optional)"
      ))
    }

    cat("\n=== Using explicit column mapping ===\n")

    # Extract data from mapped columns
    id <- plots[[id_mapped]]
    genus <- plots[[genus_mapped]]
    species <- plots[[species_mapped]]
    diameter <- as.numeric(plots[[diameter_mapped]])
    x <- as.numeric(plots[[x_mapped]])
    y <- as.numeric(plots[[y_mapped]])
    size <- as.numeric(plots[[size_mapped]])
    year <- as.numeric(plots[[year_mapped]])
    fez <- NA
    gez <- NA

    cat("Plot ID:", id_mapped, "\n")
    cat("Genus:", genus_mapped, "\n")
    cat("Species:", species_mapped, "\n")
    cat("Diameter:", diameter_mapped, "\n")

    # Handle optional height column
    if (!is.null(height_mapped) && height_mapped %in% valid_cols) {
      height <- as.numeric(plots[[height_mapped]])
      cat("Height:", height_mapped, "\n")
      plt <- data.frame(id, genus, species, diameter, height, size, fez, gez, year)
    } else {
      cat("Height: Not provided\n")
      plt <- data.frame(id, genus, species, diameter, size, fez, gez, year)
    }

    cat("Longitude:", x_mapped, "\n")
    cat("Latitude:", y_mapped, "\n")
    cat("Size:", size_mapped, "\n")
    cat("Year:", year_mapped, "\n")
    cat("===================================\n\n")

    plt1 <- data.frame(id, x, y)

    # Optionally recode the unique Plot ID as numeric factors
    plt$id  <- as.numeric(as.factor(plt$id))
    plt1$id <- as.numeric(as.factor(plt1$id))

    return(list(plt, plt1))
  }

  # Check if interactive mode is available
  if (!allow_interactive && !interactive()) {
    stop(paste0(
      "RawPlotsTree requires interactive input or allow_interactive = TRUE.\n",
      "For tree-level data formatting in non-interactive mode, please pre-format your data ",
      "or run in an interactive session, or use column_map parameter."
    ))
  }

  # Helper function for simple column selection
  get_column <- function(prompt) {
    choice <- menu(valid_cols, title = prompt)
    colname <- valid_cols[choice]
    return(plots[[colname]])
  }

  id      <- get_column("Which column is your unique Plot ID?")
  genus   <- get_column("Column of tree Genus?")
  species <- get_column("Column of tree Species?")
  diameter <- as.numeric(get_column("Column of tree DBH?"))

  ans <- menu(c('yes', 'no'), title = "Is tree height data available?")
  if (ans == 1) {
    height <- as.numeric(get_column("Column of tree Height?"))
  }

  x    <- as.numeric(get_column("Column for longitude?"))
  y    <- as.numeric(get_column("Column for latitude?"))
  size <- as.numeric(get_column("Plot size column?"))
  fez  <- NA
  gez  <- NA
  year <- as.numeric(get_column("Year column?"))

  if (ans == 1) {
    plt <- data.frame(id, genus, species, diameter, height, size, fez, gez, year)
  } else {
    plt <- data.frame(id, genus, species, diameter, size, fez, gez, year)
  }

  plt1 <- data.frame(id, x, y)

  # Optionally recode the unique Plot ID as numeric factors
  plt$id  <- as.numeric(as.factor(plt$id))
  plt1$id <- as.numeric(as.factor(plt1$id))

  return(list(plt, plt1))
}
