## Updates made to the new framework:
# 30/04/2025:
# - Migrated from scripts/ToDatabase.R
# - Added proper roxygen documentation
# - Improved file path handling to avoid working directory manipulation
# - Added input validation and better error handling


#' Integrate Plot Dataset to the WUR Plot Database
#'
#' This function prepares a plot dataset for integration into the WUR Plot Database
#' by adding required metadata and standardizing the data format.
#'
#' @param plt A data frame containing plot data.
#' @param CODE Character string specifying the country/region code (e.g., 'AFR_GHA').
#' @param mapYear Numeric value specifying the map year (10, 17, or 18).
#' @param dataDir Character string specifying the directory containing biome/realm data files.
#'        Defaults to "data" if not specified.
#'
#' @return A data frame formatted for the WUR Plot Database with standardized columns.
#'
#' @importFrom sf st_as_sf st_coordinates st_transform
#' @importFrom terra rast extract
#' @importFrom dplyr left_join
#'
#' @export
#' @examples
#' \dontrun{
#' data("plots")
#' formatted_data <- ToDatabase(plots, CODE = "AFR_GHA", mapYear = 10)
#' }
ToDatabase <- function(plt, CODE = 'AFR_GHA', mapYear, dataDir = "data") {
  # Input validation
  if (!is.data.frame(plt)) {
    stop("'plt' must be a data frame")
  }
  if (!is.character(CODE)) {
    stop("'CODE' must be a character string")
  }
  if (!mapYear %in% c(10, 17, 18)) {
    stop("'mapYear' must be 10, 17, or 18")
  }
  
  # Check for required directory and files
  if (!dir.exists(dataDir)) {
    stop(paste("Data directory not found:", dataDir))
  }
  biome_file <- file.path(dataDir, "Ecoregions2017_biome.tif")
  realm_file <- file.path(dataDir, "Ecoregions2017_realm.tif")
  biome_id_file <- file.path(dataDir, "biome_id.csv")
  realm_id_file <- file.path(dataDir, "realm_id.csv")
  
  for (f in c(biome_file, realm_file, biome_id_file, realm_id_file)) {
    if (!file.exists(f)) {
      stop(paste("Required file not found:", f))
    }
  }
  
  # Assign MapName based on mapYear
  if (mapYear == 18) {
    plt$MapName <- '18_CCIBiomass'
  } else if (mapYear == 17) {
    plt$MapName <- '17_CCIBiomass'
  } else {
    plt$MapName <- '10_GlobBiomass'
  }
  
  # Define required columns
  cols <- c('CODE', 'AGB_T_HA', 'SIZE_HA', 'GEZ', 'AVG_YEAR', 'ZONE', 'POINT_X', 'POINT_Y',
            'sdTree', 'sdSE', 'AGB_T_HA_ORIG', 'sdGrowth', 'varTot', 'MapName', 'VER', 
            'sdMap', 'BIO', 'REALM', 'OPEN', 'INVENTORY', 'TIER')
  
  # Add version and openness info
  plt$VER <- 2
  plt$OPEN <- 1  # 0-open
  
  # Add inventory type
  plt$INVENTORY <- ifelse(nrow(plt) > 1000, 'regional', 'local')
  
  # Add TIER classification based on plot size
  plt$TIER <- NA
  plt$TIER[plt$SIZE_HA < 0.6] <- 'tier1'
  plt$TIER[plt$SIZE_HA >= 0.6 & plt$SIZE_HA < 3] <- 'tier2'
  plt$TIER[plt$SIZE_HA >= 3] <- 'tier3'
  
  # Extract biome and realm information
  plt0 <- plt
  if (all(c("POINT_X", "POINT_Y") %in% colnames(plt))) {
    plt_sf <- sf::st_as_sf(plt, coords = c("POINT_X", "POINT_Y"), crs = 4326)
    plt_coords <- sf::st_coordinates(plt_sf)
    
    # Load biome and realm rasters
    biome <- terra::rast(biome_file)
    realm <- terra::rast(realm_file)
    
    # Extract values from rasters
    plt_sp <- sf::st_transform(plt_sf, terra::crs(biome))
    vlsBiome <- terra::extract(biome, plt_sp)[, 2]
    vlsRealm <- terra::extract(realm, plt_sp)[, 2]
    
    plt0$BIO1 <- vlsBiome
    plt0$REALM1 <- vlsRealm
  } else {
    stop("POINT_X and POINT_Y columns required in the input data frame")
  }
  
  # Join with biome and realm ID tables
  bioID <- read.csv(biome_id_file)
  realmID <- read.csv(realm_id_file)
  
  plt <- dplyr::left_join(as.data.frame(plt0), bioID, by = c('BIO1' = 'ID'))
  plt <- dplyr::left_join(plt, realmID, by = c('REALM1' = 'ID'))
  
  # Assign biome and realm names
  plt$BIO <- plt[[length(plt) - 1]]
  plt$REALM <- plt[[length(plt)]]
  plt$sdMap <- NA
  
  # Set country/region code
  plt$CODE <- CODE
  
  # Select and order required columns
  plt <- plt[, cols]
  
  return(plt)
}