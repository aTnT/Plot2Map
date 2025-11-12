# Calculate block-to-block semivariogram

Computes the block-to-block semivariogram based on a variogram model,
discretising a block into pixels and calculating semivariances within
and between blocks at specified distances.

## Usage

``` r
BBvar(model, lat = 0, rsl, dists = c(0, 1:7 * 10 + 2))
```

## Arguments

- model:

  A variogram model object from the `gstat` package.

- lat:

  Latitude in degrees, defaults to 0.

- rsl:

  Resolution vector with x and y resolution in degrees (e.g., c(xres,
  yres)).

- dists:

  Vector of distances (in km) for which to calculate the semivariogram,
  defaults to c(0, 12, 22, 32, 42, 52, 62, 72).

## Value

An object of class `gstatVariogram` containing the block-to-block
semivariogram.

## Examples

``` r
  # Create a simple spherical variogram model and compute block semivariogram:
  library(gstat)
  vgm_model <- vgm(psill = 1, model = "Sph", range = 100, nugget = 0.1)
  res <- c(0.01, 0.01)  # 0.01-degree resolution
  result <- BBvar(model = vgm_model, lat = 40, rsl = res)
  print(result)
#>     np dist     gamma dir.hor dir.ver   id
#> 1 4950    0 0.0000000       0       0 var1
#> 2 4950   12 0.1112126       0       0 var1
#> 3 4950   22 0.2522978       0       0 var1
#> 4 4950   32 0.3894844       0       0 var1
#> 5 4950   42 0.5177378       0       0 var1
#> 6 4950   52 0.6336616       0       0 var1
#> 7 4950   62 0.7341225       0       0 var1
#> 8 4950   72 0.8160625       0       0 var1
```
