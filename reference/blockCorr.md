# Compute block covariance matrix

Calculates the covariance matrix for a block based on a variogram model,
discretising the block into pixels and computing covariances between all
pairs of points within the block.

## Usage

``` r
blockCorr(lat, rsl, model, nx, ny)
```

## Arguments

- lat:

  Latitude in degrees.

- rsl:

  Resolution vector with x and y resolution in degrees (e.g., c(xres,
  yres)).

- model:

  A variogram model object from the `gstat` package.

- nx:

  Number of pixels in the x-direction.

- ny:

  Number of pixels in the y-direction.

## Value

A numeric matrix representing the covariance between all pairs of points
within the block.

## Examples

``` r
 #Compute covariance matrix for a 5x5 pixel block:
  library(gstat)
  vgm_model <- vgm(psill = 1, model = "Sph", range = 100, nugget = 0.1)
  res <- c(0.02, 0.02)  # 0.02-degree resolution
  result <- blockCorr(lat = 40, rsl = res, model = vgm_model, nx = 5, ny = 5)
  dim(result)  # Should be 25 x 25
#> [1] 25 25
```
