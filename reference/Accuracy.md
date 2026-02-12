# Calculate accuracy metrics for AGB estimates

This function calculates accuracy metrics for Above Ground Biomass (AGB)
estimates from plot data and map data, grouped into specified intervals.
It computes means, root mean squared differences (RMSD), variances, and
other statistics per AGB bin and overall, saving the results to a CSV
file.

## Usage

``` r
Accuracy(df, intervals = 8, dir = "results", str = "")
```

## Arguments

- df:

  Dataframe containing plot and map AGB data. Must include columns with
  `plotAGB_` prefix (e.g., `plotAGB_10`) and `mapAGB`. Optionally
  include `varPlot` for uncertainty-aware validation.

- intervals:

  Number of intervals for binning AGB values. Must be 6, 7, or 8.
  Defaults to 8.

- dir:

  Directory where the results will be saved. Defaults to
  `resultsFolder`.

- str:

  String to append to the output CSV file name. Defaults to an empty
  string.

## Value

A dataframe with accuracy metrics for each AGB bin and a total row,
including:

- AGB bin (Mg/ha):

  Biomass range for the bin

- n:

  Number of observations in the bin

- AGBref (Mg/ha):

  Mean reference (plot) AGB

- AGBmap (Mg/ha):

  Mean map AGB

- RMSD:

  Root mean squared difference

- varPlot:

  Mean plot variance (uses actual varPlot if available, otherwise 1)

- AGBmap-AGBref:

  Bias (map - reference)

## Details

The function now supports uncertainty-aware validation when a `varPlot`
column is present in the input data (typically from
[`calculateTotalUncertainty()`](https://atnt.github.io/Plot2Map/reference/calculateTotalUncertainty.md)
or
[`invDasymetry()`](https://atnt.github.io/Plot2Map/reference/invDasymetry.md)).
If `varPlot` is available, it will be used to calculate proper variance
estimates and the IVar diagnostic. Otherwise, defaults to 1 for backward
compatibility.

## Examples

``` r
# \donttest{
# Example with the right column structure
# (Output of invDasymetry would include plotAGB_10 column)
example_agb_data <- data.frame(
  plotAGB_10 = c(120, 150, 180, 200, 220),
  mapAGB = c(110, 140, 190, 180, 240),
  SIZE_HA = c(0.5, 0.75, 1.0, 1.2, 0.9),
  x = c(1, 2, 3, 4, 5),
  y = c(10, 20, 30, 40, 50)
)

# Run accuracy assessment
results <- Accuracy(df = example_agb_data, intervals = 6)
print(results)
#>   AGB bin (Mg/ha) n AGBref (Mg/ha) AGBmap (Mg/ha) RMSD varPlot
#> 2         100-150 2            135            125   10       1
#> 3         150-200 2            190            185   16       1
#> 4         200-250 1            220            240   20       1
#> 7           total 5            174            172   15       1
# }
```
