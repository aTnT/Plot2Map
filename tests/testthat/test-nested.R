library(testthat)
library(Plot2Map)
library(sf)

# Tests for Nested.R

# test_that("Nested function handles sf objects correctly", {
#   # Skip interactive tests in non-interactive environments
#   testthat::skip_if_not(interactive(), "Test requires interactive menu selection")
#
#   # Create sample centroids_sf object
#   centroids <- data.frame(
#     POINT_GUID = c("P1", "P2"),
#     x = c(-62.215, -62.205),
#     y = c(-3.465, -3.455)
#   )
#   centroids_sf <- sf::st_as_sf(centroids, coords = c("x", "y"), crs = 4326)
#
#   # Create sample tree_table
#   tree_table <- data.frame(
#     POINT_GUID = c("P1", "P1", "P2"),
#     TREE_ALIVE = c(1, 1, 1),
#     TREE_OR_STUMP = c(1, 1, 1),
#     DBH_CM = c(15, 20, 25),
#     HEIGHT_M = c(10, 12, 15),
#     GENUS = c("Pinus", "Pinus", "Quercus"),
#     SPECIES = c("sylvestris", "sylvestris", "robur")
#   )
#
#   # Expect Nested function to work with proper inputs
#   # This is commented out because it requires interactive input
#   # result <- Nested(centroids_sf, tree_table)
#   # expect_type(result, "list")
#   # expect_length(result, 2)
#
#   # Test that the function checks input types
#   expect_error(Nested(NULL, tree_table), "object .* not found")
#   expect_error(Nested(centroids_sf, NULL), "object .* not found")
# })
