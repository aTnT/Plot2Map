# Format Plot Data

This function formats raw plot data into a standardized structure for
further processing. It can automatically detect common column names or
prompt for user input in interactive sessions.

## Usage

``` r
RawPlots(plots, mapYear = NULL, allow_interactive = TRUE)
```

## Arguments

- plots:

  A data frame containing plot data with Latitude, Longitude
  coordinates.

- mapYear:

  Optional. The year of the map being used for comparison (not used in
  current implementation).

- allow_interactive:

  Logical. Allow interactive prompts if auto-detection fails (default:
  TRUE). Set to FALSE for automated pipelines. When FALSE and in
  non-interactive sessions, the function will attempt auto-detection and
  provide helpful error messages if columns cannot be identified.

## Value

A data frame with formatted plot data, including columns for PLOT_ID,
POINT_X, POINT_Y, AGB_T_HA, SIZE_HA, FEZ, GEZ, and AVG_YEAR.

## Details

The function attempts to auto-detect columns based on common naming
patterns:

- Plot ID: PLOT_ID, PlotID, ID, plot_id, plotid, etc. (if not found,
  generates sequential IDs)

- AGB: AGB_T_HA, AGB, agb, biomass, etc. (required)

- Longitude: longitude, lon, long, x, POINT_X, etc. (required)

- Latitude: latitude, lat, y, POINT_Y, etc. (required)

- Size: SIZE_HA, size, area, plot_size, plotsize, etc. (required)

- Year: AVG_YEAR, year, YEAR, measurement_year, etc. (required)

If auto-detection fails for required columns and allow_interactive is
TRUE in an interactive session, it prompts the user to select columns
manually. In non-interactive environments with allow_interactive =
FALSE, it provides an error listing available columns.

## Examples

``` r
if (FALSE) { # \dontrun{
# Auto-detection (works in both interactive and non-interactive)
formatted_plots <- RawPlots(raw_plots)

# Non-interactive mode (for scripts/pipelines)
formatted_plots <- RawPlots(raw_plots, allow_interactive = FALSE)
} # }
```
