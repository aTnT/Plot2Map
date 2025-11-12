# Format Plot Data

This function formats raw plot data into a standardized structure for
further processing.

## Usage

``` r
RawPlots(plots, mapYear = NULL)
```

## Arguments

- plots:

  A data frame containing plot data with Latitude, Longitude
  coordinates.

- mapYear:

  Optional. The year of the map being used for comparison (not used in
  current implementation).

## Value

A data frame with formatted plot data, including columns for PLOT_ID,
POINT_X, POINT_Y, AGB_T_HA, SIZE_HA, FEZ, GEZ, and AVG_YEAR.

## Details

The function prompts the user to select or manually enter column indices
for key plot attributes. It then formats the data, converting plot sizes
from m^2 to hectares if necessary.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'raw_plots' is your input data frame
formatted_plots <- RawPlots(raw_plots)
} # }
```
