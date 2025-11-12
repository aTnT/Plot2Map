# Assign strata weights to circular plots

Assigns predefined strata weights to circular plots based on their
strata classification. This is an internal helper function used by
`StrataAGB` and is not intended for direct use.

## Usage

``` r
assign_strata_weights(plots, strata_weights = c(A = 0.22, B = 0.22, C = 0.68))
```

## Arguments

- plots:

  A data frame containing plot data with at least a `stratum` column
  (character or factor).

- strata_weights:

  A named numeric vector where names are strata levels and values are
  weights. Defaults to `c("A" = 0.22, "B" = 0.22, "C" = 0.68)`.

## Value

A data frame with an additional `wt` column containing the assigned
weights.

## Examples

``` r
  # Example: Assign weights to a simple plot data frame
  strata_plots <- data.frame(stratum = c("A", "B", "C"))
  weighted_plots <- assign_strata_weights(strata_plots)
  print(weighted_plots)
#>   stratum   wt
#> 1       A 0.22
#> 2       B 0.22
#> 3       C 0.68
```
