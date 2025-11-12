# Format Tree-Level Plot Data

This function formats raw tree-level plot data into a standardized
structure for further processing.

## Usage

``` r
RawPlotsTree(plots)
```

## Arguments

- plots:

  A data frame containing tree-level plot data with Latitude, Longitude
  coordinates.

## Value

A list containing two data frames:

1.  Tree-level data with columns for id, genus, species, diameter,
    (height), size, fez, gez, year

2.  Plot-level data with columns for id, x, y

## Details

The function prompts the user to select column indices for key tree and
plot attributes. It handles cases with and without tree height data.

## Examples

``` r
if (FALSE) { # \dontrun{
# This function requires interactive input
# Sample code to format tree-level data:
tree_data <- read.csv(sample_file("SampleTreeNested.csv"))
formatted_tree_plots <- RawPlotsTree(tree_data)
} # }
```
