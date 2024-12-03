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
#' @importFrom sf st_polygon st_sfc st_sf st_crs st_point st_buffer st_make_valid st_as_sf st_transform st_coordinates st_area
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
  dat <- split(df, df$id)
  pol <- lapply(dat, function(x) polyIrreg(x))
  pol1 <- st_make_valid(st_as_sf(do.call(rbind, pol)))
  st_crs(pol1) <- st_crs(SRS)

  pol1$PLOT_ID <- row.names(pol1)
  pol1$SIZE_HA <- round(st_area(pol1) / 10000, 2)
  c <- st_centroid(pol1)

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
#
# # Testing the new vs old framework
#
# library(testthat)
# library(sf)
# library(dplyr)
#
# # Define old and new functions here or source them from files
# # Assuming old_Polygonize and Polygonize are defined
#
# test_that("Polygonize function produces consistent results between old and new framework", {
#   # Test case 1: Regular rectangular plot
#   test_data1 <- data.frame(
#     id = rep("plot1", 4),
#     POINT_X = c(0, 1, 1, 0),
#     POINT_Y = c(0, 0, 1, 1)
#   )
#
#   old_result1 <- old_Polygonize(test_data1, 4326)
#   old_result1_sf <- st_as_sf(old_result1, coords = c("POINT_X", "POINT_Y"), crs = 4326)
#   new_result1 <- Polygonize(test_data1, 4326)
#   new_result1_sf <- st_as_sf(new_result1, coords = c("POINT_X", "POINT_Y"), crs = 4326)
#
#   expect_equal(old_result1, new_result1)
#
#   # Check for valid geometries
#   expect_true(all(st_is_valid(st_as_sf(old_result1_sf))))
#   expect_true(all(st_is_valid(st_as_sf(new_result1_sf))))
#
#   # Test case 2: Irregular plot
#   test_data2 <- data.frame(
#     id = rep("plot2", 5),
#     POINT_X = c(0, 1, 2, 1, 0),
#     POINT_Y = c(0, 0, 1, 2, 1)
#   )
#
#   old_result2 <- old_Polygonize(test_data2, 4326)
#   new_result2 <- Polygonize(test_data2, 4326)
#
#   expect_equal(old_result2, new_result2)
#
#   # Test case 3: Multiple plots
#   test_data3 <- data.frame(
#     id = c(rep("plot1", 4), rep("plot2", 4)),
#     POINT_X = c(0, 1, 1, 0, 2, 3, 3, 2),
#     POINT_Y = c(0, 0, 1, 1, 2, 2, 3, 3)
#   )
#
#   old_result3 <- old_Polygonize(test_data3, 4326)
#   old_result3_sf <- st_as_sf(old_result3, coords = c("POINT_X", "POINT_Y"), crs = 4326)
#
#   if (!all(st_is_valid(old_result3_sf))) {
#     stop("Old result contains invalid geometries")
#   }
#
#   new_result3 <- Polygonize(test_data3, 4326)
#   new_result3_sf <- st_as_sf(new_result3, coords = c("POINT_X", "POINT_Y"), crs = 4326)
#
#   if (!all(st_is_valid(new_result3_sf))) {
#     stop("new result contains invalid geometries")
#   }
#
#   expect_equal(old_result3$PLOT_ID, new_result3$PLOT_ID)
#   expect_equal(old_result3$SIZE_HA, new_result3$SIZE_HA)
#   expect_equal(old_result3$POINT_X, new_result3$POINT_X)
#   expect_equal(old_result3$POINT_Y, new_result3$POINT_Y)
#
# })



