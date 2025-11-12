# Assign ecological zones and continents to plot locations

This function overlays plot locations with pre-processed data to assign
corresponding FAO ecological zones (biomes), Global Ecological Zones
(GEZ) and continents (zones) to each plot.

## Usage

``` r
BiomePair(plt)
```

## Arguments

- plt:

  A data frame or sf object containing plot data. For data frame input
  format, longitude and latitude coordinates should be placed under
  "POINT_X" and "POINT_Y" columns respectively in WGS 84 CRS.

## Value

A data frame with added columns for ZONE (continent), FAO.ecozone, and
GEZ (Global Ecological Zones).

## Examples

``` r
if (FALSE) { # \dontrun{
  plot_data <- data.frame(POINT_X = c(-1.007, -1.208), POINT_Y = c(12.010, 13.611))
  result <- BiomePair(plot_data)
} # }
```
