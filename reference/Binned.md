# Create a binned plot of AGB data

This function creates a binned plot of Above Ground Biomass (AGB) data,
with points sized according to the number of observations and error bars
showing the interquartile range.

## Usage

``` r
Binned(x, y, caption = "", fname = "", outDir = "results")
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
Binned(ref_data, map_data, "Example Plot", "binned_plot.png")
} # }
```
