## Updates made to the new framework:
# 20/03/25:
# Update to reflect commit b9000c1 at https://github.com/arnanaraza/Plot2Map
# 07/07/25:
# Fixed bug in RawPlots causing PLOT_ID to become NA when selecting parcel id column


#' Format Plot Data
#'
#' This function formats raw plot data into a standardized structure for further processing.
#'
#' @param plots A data frame containing plot data with Latitude, Longitude coordinates.
#' @param mapYear Optional. The year of the map being used for comparison (not used in current implementation).
#'
#' @return A data frame with formatted plot data, including columns for PLOT_ID, POINT_X, POINT_Y, AGB_T_HA, SIZE_HA, FEZ, GEZ, and AVG_YEAR.
#'
#' @details
#' The function prompts the user to select or manually enter column indices for key plot attributes.
#' It then formats the data, converting plot sizes from m^2 to hectares if necessary.
#'
#' @examples
#' \dontrun{
#' # Assuming 'raw_plots' is your input data frame
#' formatted_plots <- RawPlots(raw_plots)
#' }
#' @export
RawPlots <- function(plots, mapYear = NULL) {
  # Type checking
  if (!is.data.frame(plots)) {
    stop('Input file should be a data frame with XY coordinates')
  }

  # Helper function for column selection or manual entry
  select_column <- function(prompt) {
    # Exclude list columns (e.g., geometry in spatial data)
    valid_cols <- names(plots)[!sapply(plots, is.list)]
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

  id <- select_column("Which column is your unique Plot ID?")
  agb <- select_column("Which column is your plot AGB?")
  x <- select_column("Select longitude column")
  y <- select_column("Which column is your latitude?")
  size <- select_column("Select plot size column")
  year <- select_column("Select year column")

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
#'
#' @return A list containing two data frames:
#'   1. Tree-level data with columns for id, genus, species, diameter, (height), size, fez, gez, year
#'   2. Plot-level data with columns for id, x, y
#'
#' @details
#' The function prompts the user to select column indices for key tree and plot attributes.
#' It handles cases with and without tree height data.
#'
#' @examples
#' \dontrun{
#' # This function requires interactive input
#' # Sample code to format tree-level data:
#' tree_data <- read.csv(sample_file("SampleTreeNested.csv"))
#' formatted_tree_plots <- RawPlotsTree(tree_data)
#' }
#'
#' @export
RawPlotsTree <- function(plots) {
  if (!is.data.frame(plots)) {
    stop('Input file should be a data frame with XY coordinates')
  }

  # Exclude list columns from selection
  valid_cols <- names(plots)[!sapply(plots, is.list)]

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
