# Format Tree-Level Plot Data

This function formats raw tree-level plot data into a standardized
structure for further processing.

## Usage

``` r
RawPlotsTree(plots, allow_interactive = TRUE, column_map = NULL)
```

## Arguments

- plots:

  A data frame containing tree-level plot data with Latitude, Longitude
  coordinates.

- allow_interactive:

  Logical. Allow interactive prompts if auto-detection fails (default:
  TRUE). Set to FALSE for automated pipelines.

- column_map:

  Optional named list explicitly specifying column names. Bypasses
  interactive prompts. Use names: id, genus, species, diameter, height
  (optional), x, y, size, year. Example:
  `column_map = list(id = "PlotCode", genus = "Genus", species = "Species", diameter = "DBH_cm", height = "Height_m", x = "Longitude", y = "Latitude", size = "PlotArea_ha", year = "MeasYear")`.

## Value

A list containing two data frames:

1.  Tree-level data with columns for id, genus, species, diameter,
    (height), size, fez, gez, year

2.  Plot-level data with columns for id, x, y

## Details

This function currently requires interactive input or properly named
columns. For non-interactive use, ensure columns have standard names or
use `allow_interactive = FALSE` with appropriate column names.

Alternatively, use `column_map` to explicitly specify columns, bypassing
interactive prompts entirely.

## Examples

``` r
if (FALSE) { # \dontrun{
# Interactive mode
tree_data <- read.csv(sample_file("SampleTreeNested.csv"))
formatted_tree_plots <- RawPlotsTree(tree_data)

# Non-interactive mode (requires standard column names)
formatted_tree_plots <- RawPlotsTree(tree_data, allow_interactive = FALSE)

# Explicit column mapping
formatted_tree_plots <- RawPlotsTree(tree_data,
  allow_interactive = FALSE,
  column_map = list(
    id = "PlotCode",
    genus = "Genus",
    species = "Species",
    diameter = "DBH_cm",
    height = "Height_m",
    x = "Longitude",
    y = "Latitude",
    size = "PlotArea_ha",
    year = "MeasYear"
  ))
} # }
```
