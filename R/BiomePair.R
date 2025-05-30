## Updates made to the new framework:
# 07/12/2024:
# Uses terra package functions consistently for spatial operations.
# Assumes that the shapefiles are included in the data directory.
# 10/04/2025:
# Added (commented) checks/tests that X and Y points are contained within geometry


#' Assign ecological zones and continents to plot locations
#'
#' This function overlays plot locations with pre-processed data to assign
#' corresponding FAO ecological zones (biomes), Global Ecological Zones (GEZ)
#' and continents (zones) to each plot.
#'
#' @inheritParams Deforested
#'
#' @return A data frame with added columns for ZONE (continent), FAO.ecozone, and GEZ (Global Ecological Zones).
#'
#' @importFrom terra vect intersect
#' @importFrom stringr word
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   plot_data <- data.frame(POINT_X = c(-1.007, -1.208), POINT_Y = c(12.010, 13.611))
#'   result <- BiomePair(plot_data)
#'}
#'
BiomePair <- function(plt) {

  plt <- check_and_convert_plt(plt)

  # Convert input to terra vector
  plots0 <- terra::vect(plt, geom = c("POINT_X", "POINT_Y"))

  # Load pre-processed shapefiles using sample_file function
  li <- terra::vect(sample_file("eco_zone.shp"))
  re <- terra::vect(sample_file("world_region.shp"))

  # # Prepare points for intersection
  p <- plots0
  p$POINT_X <- plt$POINT_X
  p$POINT_Y <- plt$POINT_Y

  # # Check if points are contained within plots0 geometry
  # is_contained <- terra::is.related(terra::vect(matrix(c(p$POINT_X, p$POINT_Y),
  #                                                      ncol=2),
  #                                               type="points"),
  #                                   plots0,
  #                                   "within")
  #
  # # Add validation check and warning
  # if (!all(is_contained)) {
  #   warning("Some points (POINT_X, POINT_Y) are not contained within plots0 geometry")
  #   # Optionally subset to valid points only
  #   p <- p[is_contained, ]
  #   if (nrow(p) == 0) {
  #     stop("No valid points remaining after containment check")
  #   }
  # }

  # Intersect polygons with points
  intFez0 <- terra::intersect(p, li)
  df <- terra::intersect(re, intFez0)

  # Prepare and clean data
  df$ZONE <- as.character(df$SUBREGION)
  df$FAO.ecozone <- as.character(df$GEZ_TERM)
  df$GEZ <- stringr::word(df$FAO.ecozone, 1)
  df <- df[, !names(df) %in% c("SUBREGION", "GEZ_TERM", 'FEZ', 'ORIG_FID')]

  # Clean up GEZ and FAO.ecozone
  df$GEZ <- ifelse(df$GEZ == 'Polar', 'Boreal', df$GEZ)
  df$FAO.ecozone <- ifelse(df$FAO.ecozone == 'Polar', 'Boreal coniferous forest', df$FAO.ecozone)

  # Remove water areas and clean up ZONE names
  df <- subset(df, df$GEZ != 'Water')
  df$ZONE <- ifelse(stringr::word(df$ZONE, 1) == 'Australia', 'Australia', df$ZONE)
  df$ZONE <- ifelse(stringr::word(df$ZONE, 1) %in% c('South', 'America'), 'S.America', df$ZONE)
  df$ZONE <- ifelse(stringr::word(df$ZONE, 1) %in% c('Central', 'America'), 'C.America', df$ZONE)
  df$ZONE <- ifelse(stringr::word(df$ZONE, 2) == 'Asia' & !is.na(stringr::word(df$ZONE, 2)), 'Asia', df$ZONE)
  df$ZONE <- ifelse(stringr::word(df$ZONE, 2) == 'Africa' & !is.na(stringr::word(df$ZONE, 2)), 'Africa', df$ZONE)
  df$ZONE <- ifelse(stringr::word(df$ZONE, 2) == 'Europe' & !is.na(stringr::word(df$ZONE, 2)), 'Europe', df$ZONE)

  return(as.data.frame(df))
}



# ### FUNCTION TO GET THE CORRESPONDING ZONES AND BIOMES OF PLOT LOCATIONS USING PRE-PROCESSED SHAPEFILES
#
# old_BiomePair <- function(df_defoCheck){
#
#   plots0 <- df_defoCheck
#   plots0 <- vect(plots0, geom=c("POINT_X", "POINT_Y"))
#
#   li <- vect("data/eco_zone.shp")
#   re <- vect("data/world_region.shp")
#
#   ## convert it to 'terra'
#   p <- plots0
#   p$POINT_X <- df_defoCheck$POINT_X
#   p$POINT_Y <- df_defoCheck$POINT_Y
#   ## intersect polygons with points, keeping the information from both
#   intFez0 <- terra::intersect(p, li)
#   df <- terra::intersect(re, intFez0)
#
#   #order first before joining
#   df$ZONE <- as.character(df$SUBREGION)
#   df$FAO.ecozone <- as.character(df$GEZ_TERM)
#   df$GEZ <- word(df$FAO.ecozone, 1)
#   df <- df[, -which(names(df) %in% c("SUBREGION", "GEZ_TERM", 'FEZ', 'ORIG_FID'))]
#
#   #some cleaning
#   df$GEZ <- ifelse(df$GEZ == 'Polar', 'Boreal', df$GEZ)
#   df$FAO.ecozone <- ifelse(df$FAO.ecozone == 'Polar', 'Boreal coniferous forest', df$FAO.ecozone)
#
#   df <- subset(df, df$GEZ != 'Water')# | df$GEZ != 'No')
#   df$ZONE <- ifelse(word(df$ZONE, 1) == 'Australia', 'Australia', df$ZONE)
#   df$ZONE <- ifelse(word(df$ZONE, 1) == 'South' | word(df$ZONE, 1) == 'America' , 'S.America', df$ZONE)
#   df$ZONE <- ifelse(word(df$ZONE, 1) == 'Central' | word(df$ZONE, 1) == 'America' , 'C.America', df$ZONE)
#   df$ZONE <- ifelse(word(df$ZONE, 2) == 'Asia' & !is.na(word(df$ZONE, 2)), 'Asia', df$ZONE)
#   df$ZONE <- ifelse(word(df$ZONE, 2) == 'Africa' & !is.na(word(df$ZONE, 2)),'Africa', df$ZONE)
#   df$ZONE <- ifelse(word(df$ZONE, 2) == 'Europe' & !is.na(word(df$ZONE, 2)),'Europe', df$ZONE)
#
#   plt <- as.data.frame(df)
#   return(plt)
# }



