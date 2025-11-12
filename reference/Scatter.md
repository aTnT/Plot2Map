# Create a scatter plot of AGB data

This function creates a simple scatter plot of Above Ground Biomass
(AGB) data with a 1:1 line.

## Usage

``` r
Scatter(x, y, caption = "", fname = "", outDir = "results")
```

## Arguments

- x:

  Numeric vector of reference AGB values.

- y:

  Numeric vector of mapped AGB values (same length as x).

- caption:

  Character string for the plot title/caption.

- fname:

  Character string specifying the output file name for saving the plot.
  If empty, the plot will be displayed but not saved.

- outDir:

  Character string specifying the output directory for saving the plot
  (default: "results").

## Value

Invisibly returns NULL. The function creates a plot as a side effect.

## Examples

``` r
if (FALSE) { # \dontrun{
data("plots")
set.seed(42)
ref_data <- plots$AGB_T_HA[1:100]
map_data <- ref_data + rnorm(100, 0, 20)
Scatter(ref_data, map_data, "Example Scatter Plot", "scatter_plot.png")
} # }
```
