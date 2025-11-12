# Create polygons from plot coordinates

This function creates polygons from subplot corner coordinates or
irregular plot shapes, converting from a source SRS to EPSG:4326 /
WSG84. It can handle both rectangular and non-rectangular plots, as well
as circular plots.

## Usage

``` r
Polygonize(df, SRS)
```

## Arguments

- df:

  A data frame containing plot coordinates and identification labels.

- SRS:

  The Spatial Reference System to assign to the resulting polygons.

## Value

A data frame with polygon information, including PLOT_ID, SIZE_HA,
POINT_X, and POINT_Y.

## Examples

``` r
if (FALSE) { # \dontrun{
  plot_data <- data.frame(
    id = c(rep("plot1", 4), rep("plot2", 4)),
    POINT_X = c(0, 1, 1, 0, 2, 3, 3, 2),
    POINT_Y = c(0, 0, 1, 1, 2, 2, 3, 3)
  )
  polygons <- Polygonize(plot_data, 4326)
} # }
```
