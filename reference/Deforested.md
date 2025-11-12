# Remove deforested plots by overlaying plots with Global Forest Change data

This function identifies and removes plots that have been deforested
based on the Global Forest Change (GFC) dataset (Hansen et al., 2013).
It processes each plot, downloads the necessary forest loss tiles, and
determines if the plot has been deforested beyond a set deforestation
threshold or if the deforestation occurred before or during the
specified map year.

## Usage

``` r
Deforested(
  plt,
  map_year,
  gfc_folder = "data/GFC",
  gfc_dataset_year = "latest",
  defo_threshold = 0.05
)
```

## Arguments

- plt:

  A data frame or sf object containing plot data. For data frame input
  format, longitude and latitude coordinates should be placed under
  "POINT_X" and "POINT_Y" columns respectively in WGS 84 CRS.

- map_year:

  Numeric value indicating the threshold year for deforestation. Plots
  with deforestation started at or before the `map_year` will be removed
  from the `$non_deforested_plots` list element output. Any year in the
  2001-2023 range.

- gfc_folder:

  Character string specifying the directory to download GFC data.

- gfc_dataset_year:

  Numeric value describing which version of the Hansen data to use: any
  year in the 2018-2023 range or "latest" (default).

- defo_threshold:

  Numeric value indicating the deforestation threshold. Plots with a
  deforestation area proportion larger than the set `defo_threshold`
  will be removed from the `$non_deforested_plots` list element output.
  Default is 5%.

## Value

A list containing two elements:

- non_deforested_plots:

  A sf object with non-deforested plots

- all_plots:

  The original input sf object with added deforestated proportion (0-1)
  w.r.t. to plot area and deforestation start year

## References

M. C. Hansen et al., High-Resolution Global Maps of 21st-Century Forest
Cover Change. Science342,850-853(2013).
[DOI:10.1126/science.1244693](https://doi.org/10.1126/science.1244693)

## Examples

``` r
if (FALSE) { # \dontrun{
# 4 plots without and 4 plots with deforestation:
plots_sample <- c(1, 2, 3, 4, 182, 200, 323, 6765)
sampled_plots <- plots[plots_sample,]
Deforested(sampled_plots, 2010)
} # }
```
