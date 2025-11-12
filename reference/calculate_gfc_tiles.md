# Calculate the GFC product tiles needed for a given area

Determines which Global Forest Change (GFC) product tiles intersect with
a specified area of interest.

## Usage

``` r
calculate_gfc_tiles(aoi, recreate_grid = FALSE)
```

## Arguments

- aoi:

  An sf object representing the Area of Interest

- recreate_grid:

  Logical. If TRUE, force recreation of the GFC tiles grid

## Value

A filtered sf object containing only the tiles that intersect with the
AOI

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)

# Create a polygon in Central Africa
aoi <- st_sf(geometry = st_sfc(st_polygon(list(cbind(
  c(20, 21, 21, 20, 20),
  c(-1, -1, 1, 1, -1)))), crs = 4326))

tiles <- calculate_gfc_tiles(aoi)
} # }
```
