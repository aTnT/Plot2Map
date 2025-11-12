# Format tree-level data from nested plots

This function processes tree-level data from nested plots, combining
centroid information with tree measurements using a unique ID from these
datasets.

## Usage

``` r
Nested(centroids_sf, tree_table)
```

## Arguments

- centroids_sf:

  A sf object containing centroids information of sub-plots.

- tree_table:

  A data frame containing tree-level measurements.

## Value

A list containing two elements:

- plotTree:

  A data frame with formatted tree-level data

- xyTree:

  A data frame with plot ID and coordinates

## Details

The function performs the following steps:

1.  Converts centroid data to WGS84 projection

2.  Creates a buffer around centroids

3.  Filters tree data for alive trees

4.  Prompts user for column selections and additional information

5.  Processes tree height data

6.  Combines tree data with centroid coordinates

## Examples

``` r
if (FALSE) { # \dontrun{
# This example requires interactive input and spatial data files
# Load centroid data (sf object)
centroid_data <- sf::st_read(system.file("extdata", "SampleCentroid.shp", package = "Plot2Map"))
# Load tree data
tree_data <- read.csv(system.file("extdata", "SampleTreeNested.csv", package = "Plot2Map"))
# Process the nested plot data
nested_data <- Nested(centroid_data, tree_data)
} # }
```
