## Updates made to the new framework:
# 06/01/2025:
# Replaces the terra package with sf, which is more commonly used for vector data in R and is CRAN-compatible.
# Uses sf::st_polygon() to create the polygon geometry.
# Wraps the polygon in an sf object using sf::st_sf() and sf::st_sfc().
# Sets the CRS to WGS84 (EPSG:4326) directly in the sf object creation.

## Notes:



#' Create a block polygon
#'
#' This function creates a square polygon (block) based on given coordinates and size.
#' It calculates the lower left corner of the cell and defines the vertices of the polygon.
#'
#' @param x Numeric, x-coordinate of a point within the desired block.
#' @param y Numeric, y-coordinate of a point within the desired block.
#' @param size Numeric, the size of the square block.
#'
#' @return An sf object representing the square polygon.
#'
#' @importFrom sf st_polygon st_sfc st_sf
#'
#' @export
#'
#' @examples
#' block <- MakeBlockPolygon(5.2, 10.7, 1)
MakeBlockPolygon <- function(x, y, size) {
  # Calculate the lower left corner of the cell
  xll <- size * (x %/% size)
  yll <- size * (y %/% size)

  # Define the vertices of the polygon
  vertices <- rbind(
    c(xll, yll),
    c(xll + size, yll),
    c(xll + size, yll + size),
    c(xll, yll + size),
    c(xll, yll) # Close the polygon by repeating the first point
  )

  # Create an sf polygon
  polygon <- sf::st_polygon(list(vertices))

  # Create an sf object with the polygon
  sf_polygon <- sf::st_sf(geometry = sf::st_sfc(polygon, crs = 4326))

  return(sf_polygon)
}



# # make Polygon containing x, y and aligning to AGB map pixel
# # or larger cell over which AGB is aggregated
#
# # MakeBlockPolygon <- function(x, y, size){
# #   xll <- size * x %/% size
# #   yll <- size * y %/% size
# #   pol0 <- Polygon(cbind(c(xll, xll+size, xll+size, xll, xll),
# #                         c(yll, yll, yll+size, yll+size, yll)))
# #   pol1 <- Polygons(list(pol0), "pol")
# #   return(SpatialPolygons(list(pol1), proj4string=SRS))
# # }
#
#
# # make Polygon containing x, y and aligning to AGB map pixel
# # or larger cell over which AGB is aggregated
# old_MakeBlockPolygon <- function(x, y, size) {
#   # Calculate the lower left corner of the cell
#   xll <- size * (x %/% size)
#   yll <- size * (y %/% size)
#
#   # Define the vertices of the polygon
#   vertices <- rbind(
#     c(xll, yll),
#     c(xll + size, yll),
#     c(xll + size, yll + size),
#     c(xll, yll + size),
#     c(xll, yll) # Close the polygon by repeating the first point
#   )
#
#   # Use terra to create a SpatVector for the polygon
#   # Create a matrix with coordinates for lines to form a polygon
#   mat <- rbind(vertices, vertices[1,]) # Ensure the polygon is closed by repeating the first vertex
#
#   # Create a SpatVector from the coordinates
#   pol <- vect(mat, type = "polygons", crs = "+proj=longlat +datum=WGS84")
#
#   return(pol)
# }



# Tests:

# library(testthat)
# library(sf)
# library(terra)
#
#
# # Test comparison between old and new versions
# test_that("Old and new MakeBlockPolygon functions produce consistent results", {
#   # Test cases
#   x <- c(5.2, 10.7, -3.5)
#   y <- c(10.7, 15.3, 4.2)
#   size <- c(1, 2, 0.5)
#
#   for (i in 1:length(x)) {
#     old_result <- old_MakeBlockPolygon(x[i], y[i], size[i])
#     new_result <- MakeBlockPolygon(x[i], y[i], size[i])
#
#     # Convert old result to sf for comparison
#     old_sf <- sf::st_as_sf(old_result) |>
#       sf::st_set_crs(4326)
#
#     # Compare geometries
#     #expect_true(st_equals(old_sf, new_result))
#
#     # Compare bounding boxes
#     expect_equal(st_bbox(old_sf), st_bbox(new_result))
#   }
# })
#
# # Test internal consistency
# test_that("MakeBlockPolygon function behaves consistently", {
#   x <- 5
#   y <- 10
#   size <- 1
#
#   result <- MakeBlockPolygon(x, y, size)
#
#   # Check output structure
#   expect_s3_class(result, "sf")
#   expect_true("geometry" %in% names(result))
#
#   # Check if the polygon is square
#   bbox <- st_bbox(result)
#   expect_equal(as.numeric(- bbox["xmin"] + bbox["xmax"]), as.numeric(- bbox["ymin"] + bbox["ymax"]))
#
#   # Check if the polygon size is correct
#   expect_equal(as.numeric(bbox["xmax"] - bbox["xmin"]), size)
#
#   # Check if the polygon contains the input point
#   # point <- st_point(c(x, y))
#   # expect_true(st_contains(result, st_sfc(point, crs = 4326))[[1]])
#
#   # Check CRS
#   expect_equal(st_crs(result)$epsg, 4326)
# })

