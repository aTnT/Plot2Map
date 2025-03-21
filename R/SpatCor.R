## Updates made to the new framework:
# 20/03/25:
# Specified exact imports using @importFrom to avoid namespace pollution
# Included reproducible example with package data references
# Improved code formatting with consistent spacing and assignment operators
# Maintained internal helper functions without exporting them



#' Calculate block-to-block semivariogram
#'
#' Computes the block-to-block semivariogram based on a variogram model, discretising a block into pixels
#' and calculating semivariances within and between blocks at specified distances.
#'
#' @param model A variogram model object from the `gstat` package.
#' @param lat Latitude in degrees, defaults to 0.
#' @param rsl Resolution vector with x and y resolution in degrees (e.g., c(xres, yres)).
#' @param dists Vector of distances (in km) for which to calculate the semivariogram,
#'              defaults to c(0, 12, 22, 32, 42, 52, 62, 72).
#'
#' @return An object of class `gstatVariogram` containing the block-to-block semivariogram.
#' @importFrom geosphere distCosine
#' @importFrom gstat variogramLine
#' @export
#' @examples
#'   # Create a simple spherical variogram model and compute block semivariogram:
#'   library(gstat)
#'   vgm_model <- vgm(psill = 1, model = "Sph", range = 100, nugget = 0.1)
#'   res <- c(0.01, 0.01)  # 0.01-degree resolution
#'   result <- BBvar(model = vgm_model, lat = 40, rsl = res)
#'   print(result)
BBvar <- function(model, lat = 0, rsl, dists = c(0, 1:7 * 10 + 2)) {
  nx <- round(0.1 / rsl[1], 0)
  ny <- round(0.1 / rsl[2], 0)
  n  <- nx * ny
  dx <- geosphere::distCosine(c(0, lat), c(rsl[1], lat)) / 1000  # Convert to km
  dy <- geosphere::distCosine(c(0, lat), c(0, lat + rsl[2])) / 1000  # Convert to km
  x  <- 1:nx * dx
  y  <- ceiling(ny / 2) * dy - 1:ny * dy
  pts1 <- expand.grid(x = x, y = y)  # Coordinates in km

  # Within-block semivariance
  dst <- dist(pts1)
  vgs <- matrix(NA, n, n)
  vgs[lower.tri(vgs)] <- gstat::variogramLine(model, dist_vector = dst, covariance = FALSE)$gamma
  vgs[upper.tri(vgs)] <- vgs[lower.tri(vgs)]
  diag(vgs) <- 0
  inB <- mean(vgs)

  # Block-to-block semivariances
  gamma <- numeric(length(dists))
  for (i in seq_along(dists)) {
    dst <- dists[i]
    if (dst == 0) {
      gamma[i] <- 0
    } else {
      pts2 <- pts1
      pts2$y <- pts2$y + dst
      D <- sqrt(outer(pts1$x, pts2$x, "-")^2 + outer(pts1$y, pts2$y, "-")^2)
      D_vec <- as.vector(D)
      gamma[i] <- mean(gstat::variogramLine(model, dist_vector = D_vec, covariance = FALSE)$gamma) - inB
    }
  }

  vg <- data.frame(
    np = rep(n * (n - 1) * 0.5, length(dists)),
    dist = dists,
    gamma = gamma
  )
  vg$dir.hor <- 0
  vg$dir.ver <- 0
  vg$id <- "var1"
  class(vg) <- c("gstatVariogram", "data.frame")
  vg
}

#' Compute block covariance matrix
#'
#' Calculates the covariance matrix for a block based on a variogram model, discretising the block into
#' pixels and computing covariances between all pairs of points within the block.
#'
#' @param lat Latitude in degrees.
#' @param rsl Resolution vector with x and y resolution in degrees (e.g., c(xres, yres)).
#' @param model A variogram model object from the `gstat` package.
#' @param nx Number of pixels in the x-direction.
#' @param ny Number of pixels in the y-direction.
#'
#' @return A numeric matrix representing the covariance between all pairs of points within the block.
#' @importFrom geosphere distCosine
#' @importFrom gstat variogramLine
#' @export
#' @examples
#'  #Compute covariance matrix for a 5x5 pixel block:
#'   library(gstat)
#'   vgm_model <- vgm(psill = 1, model = "Sph", range = 100, nugget = 0.1)
#'   res <- c(0.02, 0.02)  # 0.02-degree resolution
#'   result <- blockCorr(lat = 40, rsl = res, model = vgm_model, nx = 5, ny = 5)
#'   dim(result)  # Should be 25 x 25
blockCorr <- function(lat, rsl, model, nx, ny) {
  n  <- nx * ny
  dx <- geosphere::distCosine(c(0, lat), c(rsl[1], lat)) / 1000  # Convert to km
  dy <- geosphere::distCosine(c(0, lat), c(0, lat + rsl[2])) / 1000  # Convert to km
  x  <- seq(0, by = dx, length.out = nx)  # Adjusted for exact pixel positions
  y  <- seq(0, by = dy, length.out = ny)  # Adjusted for exact pixel positions
  pts <- expand.grid(x = x, y = y)
  dst <- dist(pts)
  vgs <- matrix(NA, n, n)
  vgs[lower.tri(vgs)] <- gstat::variogramLine(model, dist_vector = dst, covariance = TRUE)$gamma
  vgs[upper.tri(vgs)] <- vgs[lower.tri(vgs)]
  diag(vgs) <- sum(model$psill)
  vgs
}

#' Calculate spatial standard deviation at 0.1 degrees resolution
#'
#' Aggregates a fine-resolution raster to a coarser resolution (0.1 degrees), computing the standard
#' deviation for each block based on a variogram model, accounting for spatial correlation.
#'
#' @param vgm A variogram model object from the `gstat` package.
#' @param r A `SpatRaster` object from the `terra` package containing the input data (e.g., standard deviations).
#'
#' @return A `SpatRaster` object at 0.1-degree resolution with values representing the standard deviation
#'         of the input data aggregated over blocks, adjusted for spatial correlation.
#' @importFrom terra rast ext res ncell xFromCell yFromCell crop values setValues
#' @importFrom gstat fit.variogram
#'
#' @export
#'
#' @examples
#'   # Aggregate a synthetic raster to 0.1-degree resolution:
#'   library(terra)
#'   library(gstat)
#'   set.seed(42)
#'   r <- rast(ncol = 20, nrow = 20, xmin = 0, xmax = 1, ymin = 0, ymax = 1, vals = runif(400))
#'   plot(r)
#'   vgm_model <- gstat::vgm(psill = 1, model = "Sph", range = 100, nugget = 0.1)
#'   result <- SpatCor(vgm_model, r)
#'   plot(result)
SpatCor <- function(vgm, r) {
  vgm0 <- vgm
  ex   <- terra::ext(r)
  res  <- terra::res(r)
  lat  <- (ex[3] + ex[4]) / 2

  vgm <- BBvar(model = vgm0, lat = lat, rsl = res)
  vgm <- gstat::fit.variogram(vgm, vgm0, fit.method = 6)
  vgm[1, 2] <- 0

  c <- terra::rast(
    xmin = ex[1], xmax = ex[2],
    ymin = ex[3], ymax = ex[4],
    res = 0.1,
    crs = terra::crs(r)
  )

  pixVal <- numeric(terra::ncell(c))
  for (i in 1:terra::ncell(c)) {
    xto <- terra::xFromCell(c, i)
    yto <- terra::yFromCell(c, i)
    xmin_block <- xto - 0.05
    xmax_block <- xto + 0.05
    ymin_block <- yto - 0.05
    ymax_block <- yto + 0.05

    block_ext <- terra::ext(xmin_block, xmax_block, ymin_block, ymax_block)
    r_block   <- terra::crop(r, block_ext)
    nx        <- terra::ncol(r_block)
    ny        <- terra::nrow(r_block)
    vals_block <- as.vector(terra::values(r_block))  # Flatten to vector
    vals_block[is.na(vals_block)] <- 0

    CM <- blockCorr(lat = lat, rsl = res, model = vgm, nx = nx, ny = ny)

    n <- length(vals_block)
    if (nrow(CM) != n) {
      stop("Dimensions of covariance matrix (", nrow(CM), ") and block values (", n, ") do not match")
    }

    if (mean(vals_block) == 0) {
      pixVal[i] <- 0
    } else {
      pixVal[i] <- mean(outer(vals_block, vals_block) * CM)
    }
  }

  c <- terra::setValues(c, pixVal)
  sqrt(c)
}




# # Tests:
#
# # Install necessary packages if not already installed
# if (!requireNamespace("testthat", quietly = TRUE)) {
#   install.packages("testthat")
# }
# if (!requireNamespace("terra", quietly = TRUE)) {
#   install.packages("terra")
# }
# if (!requireNamespace("gstat", quietly = TRUE)) {
#   install.packages("gstat")
# }
#
# # Load required libraries
# library(testthat)
# library(terra)
# library(gstat)
#
# # Unit test with real-world raster
# test_that("SpatCor with AGB raster works", {
#   # Define the test directory (adjust this path to where the raster is located)
#
#   # Create a test directory if it doesn't exist
#   test_dir <- "tests/test_data"
#   if (!dir.exists(test_dir)) {
#     dir.create(test_dir,  recursive = TRUE)
#   }
#
#   # Construct the raster file path
#   raster_path <- file.path(test_dir, "N40W010_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2021-fv5.0.tif")
#
#   # Check if the raster file exists; skip the test if not found
#   if (!file.exists(raster_path)) {
#     skip("Raster file not found, skipping test")
#   }
#
#   # Load the raster
#   r <- terra::rast(raster_path)
#
#   # Crop the raster to a smaller extent for efficiency
#   small_ext <- terra::ext(-9.3, -9, 39, 39.3)
#   r_small <- terra::crop(r, small_ext)
#
#   # Define a simple variogram model for testing
#   vgm_model <- gstat::vgm(psill = 1, model = "Sph", range = 100, nugget = 0.1)
#
#   result <- SpatCor(vgm_model, r_small)
#
#   expect_equal(mean(terra::values(result)), 21.72897, tolerance = 1e-6)
#   expect_equal(length(terra::values(result)), 9)
#
# })


