# Add longitude and latitude columns to an sf object

This function takes an `sf` object containing point geometries and
extracts the longitude (`POINT_X`) and latitude (`POINT_Y`) coordinates
from the geometry column. These coordinates are added as new columns to
the `sf` object.

## Usage

``` r
sf_to_sf_with_coords(sf_object)
```

## Arguments

- sf_object:

  An `sf` object containing point geometries.

## Value

An updated `sf` object with additional columns: `POINT_X` (longitude)
and `POINT_Y` (latitude).

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)
# Example sf object
sampled_plots <- st_as_sf(data.frame(
  PLOT_ID = c("EU2", "EU1"),
  AGB_T_HA = c(87.87, 67.79),
  AVG_YEAR = c(2001, 2008),
  SIZE_HA = c(0.196, 0.015),
  geometry = st_sfc(st_point(c(1.305915, 42.59214)), st_point(c(15.21873, 59.81792)))
), crs = 4326)

# Convert to include POINT_X and POINT_Y
updated_sf <- sf_to_sf_with_coords(sampled_plots)
} # }
```
