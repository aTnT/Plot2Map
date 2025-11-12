# Integrate Plot Dataset to the WUR Plot Database

This function prepares a plot dataset for integration into the WUR Plot
Database by adding required metadata and standardizing the data format.

## Usage

``` r
ToDatabase(plt, CODE = "AFR_GHA", mapYear, dataDir = "data")
```

## Arguments

- plt:

  A data frame containing plot data.

- CODE:

  Character string specifying the country/region code (e.g., 'AFR_GHA').

- mapYear:

  Numeric value specifying the map year (10, 17, or 18).

- dataDir:

  Character string specifying the directory containing biome/realm data
  files. Defaults to "data" if not specified.

## Value

A data frame formatted for the WUR Plot Database with standardized
columns.

## Examples

``` r
if (FALSE) { # \dontrun{
data("plots")
formatted_data <- ToDatabase(plots, CODE = "AFR_GHA", mapYear = 10)
} # }
```
