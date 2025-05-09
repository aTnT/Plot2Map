## Updates made to the new framework:

# 04/12/2024:
# - Updated units in Polygonize() to reflect the area in ha:
# > old_result1
# ID PLOT_ID       SIZE_HA POINT_X   POINT_Y
# plot1  1   plot1 1236404 [m^2]     0.5 0.5000063
# > new_result1
# ID PLOT_ID      SIZE_HA POINT_X   POINT_Y
# plot1  1   plot1 1236404 [ha]     0.5 0.5000063


#' Create polygons from plot coordinates
#'
#' This function creates polygons from subplot corner coordinates or irregular plot shapes.
#' It can handle both rectangular and non-rectangular plots, as well as circular plots.
#'
#' @param df A data frame containing plot coordinates and identification labels.
#' @param SRS The Spatial Reference System to assign to the resulting polygons.
#'
#' @return A data frame with polygon information, including PLOT_ID, SIZE_HA, POINT_X, and POINT_Y.
#'
#' @importFrom sf st_polygon st_sfc st_sf st_crs st_point st_buffer st_make_valid st_as_sf st_transform st_coordinates st_area st_centroid
#' @importFrom dplyr select
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   plot_data <- data.frame(
#'     id = c(rep("plot1", 4), rep("plot2", 4)),
#'     POINT_X = c(0, 1, 1, 0, 2, 3, 3, 2),
#'     POINT_Y = c(0, 0, 1, 1, 2, 2, 3, 3)
#'   )
#'   polygons <- Polygonize(plot_data, 4326)
#' }
Polygonize <- function(df, SRS) {
  # Check for lwgeom package availability
  if (!requireNamespace("lwgeom", quietly = TRUE)) {
    stop("The lwgeom package is required for accurate area calculations. Please install it with install.packages('lwgeom')")
  }
  dat <- split(df, df$id)
  pol <- lapply(dat, function(x) polyIrreg(x))
  pol1 <- st_make_valid(st_as_sf(do.call(rbind, pol)))
  st_crs(pol1) <- st_crs(SRS)

  pol1$PLOT_ID <- row.names(pol1)
  pol1$SIZE_HA <- round(st_area(pol1) / 10000, 2)    # QA: keep the rounding to 2 digits?
  pol1$SIZE_HA <- units::set_units(st_area(pol1), "ha")
  # Suppress sf warnings about centroid operations on geographic coordinates
  c <- suppressWarnings(st_centroid(pol1))

  pol2 <- st_transform(c, crs = 4326)
  coords <- st_coordinates(pol2)

  pol1$POINT_X <- coords[, 1]
  pol1$POINT_Y <- coords[, 2]
  as.data.frame(pol1) %>% dplyr::select(-geometry)
}


#' Create irregular polygons
#'
#' @param coords_poly A data frame with POINT_X and POINT_Y columns.
#' @return An sf object representing the irregular polygon.
#' @keywords internal
polyIrreg <- function(coords_poly) {
  d <- as.matrix(coords_poly[, c('POINT_X', 'POINT_Y')])
  ch <- chull(d)
  coords <- d[c(ch, ch[1]), ]  # closed polygon
  sp_poly <- st_polygon(list(coords))
  sp_poly_sf <- st_sfc(sp_poly, crs = st_crs(4326))
  st_sf(geometry = sp_poly_sf, ID = 1)
}


#' Create circular plots
#'
#' @param coords_poly A data frame with POINT_X and POINT_Y columns.
#' @param radius Numeric, the radius of the circular plot in meters (default: 10).
#' @return An sf object representing the circular plot.
#' @keywords internal
polyCirc <- function(coords_poly, radius = 10) {
  center <- st_point(c(mean(coords_poly$POINT_X), mean(coords_poly$POINT_Y)))
  center_sf <- st_sfc(center, crs = st_crs(4326))
  sp_poly <- st_buffer(center_sf, dist = radius)
  st_sf(geometry = sp_poly, ID = 1)
}


# ### FUNCTION TO CREATE POLYGONS FROM SUBPLOTS WITH CORNER COORDINATES IN METERS i.e. Labriere et al. 2018
# ### AND POSSIBLE IRREGULAR PLOTS (NON-RECTANGULAR AND NON-SQUARED)
#
# # Function to create irregular polygons
# old_polyIrreg <- function(coords_poly){
#   d <- as.matrix(coords_poly[, c('POINT_X', 'POINT_Y')])
#   ch <- chull(d)
#   coords <- d[c(ch, ch[1]), ]  # closed polygon
#   sp_poly <- st_polygon(list(coords))
#   sp_poly_sf <- st_sfc(sp_poly, crs = st_crs(4326))
#   sp_poly_df <- st_sf(geometry = sp_poly_sf, ID = 1)
#   return(sp_poly_df)
# }
#
# # Function to create circular plots (as an example, you can adjust radius)
# old_polyCirc <- function(coords_poly, radius = 10){
#   center <- st_point(c(mean(coords_poly$POINT_X), mean(coords_poly$POINT_Y)))
#   center_sf <- st_sfc(center, crs = st_crs(4326))
#   sp_poly <- st_buffer(center_sf, dist = radius)
#   sp_poly_df <- st_sf(geometry = sp_poly, ID = 1)
#   return(sp_poly_df)
# }
# # Function to polygonize a data frame
# old_Polygonize <- function(df, SRS){
#   dat <- split(df, df$id)
#   pol <- lapply(dat, function(x) old_polyIrreg(x))
#   pol1 <- st_make_valid(st_as_sf(do.call(rbind, pol)))
#   st_crs(pol1) <- st_crs(SRS)
#
#    # Add PLOT_ID and SIZE_HA
#   pol1$PLOT_ID <- row.names(pol1)
#   pol1$SIZE_HA <- round(st_area(pol1) / 10000, 2)
#   c <- st_centroid(pol1)
#
#   pol2 <- st_transform(c, crs = 4326)
#   coords <-   st_coordinates(pol2)
#
#   # pol1 <- st_make_valid(pol1)# Calculate centroids
#   pol1$POINT_X <- coords[, 1]
#   pol1$POINT_Y <- coords[, 2]
#   as.data.frame(pol1) %>% dplyr::select(-geometry)
#
# }
#


