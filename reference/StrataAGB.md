# Compute weighted AGB by strata

Assigns strata weights to circular plots and computes the weighted mean
and standard deviation of above-ground biomass (AGB) and tree standard
deviation based on strata sizes.

## Usage

``` r
StrataAGB(
  plots,
  strata_weights = c(A = 0.22, B = 0.22, C = 0.68),
  verbose = TRUE
)
```

## Arguments

- plots:

  A data frame containing plot data with columns `AGB_T_HA` (numeric,
  AGB in tons per hectare), `sdTree` (numeric, standard deviation of
  tree measurements), and optionally `stratum` (character or factor,
  strata classification). If `stratum` is missing, it will be assigned
  based on the default weights.

- strata_weights:

  A named numeric vector specifying weights for each stratum. Defaults
  to `c("A" = 0.22, "B" = 0.22, "C" = 0.68)`, representing proportional
  areas.

- verbose:

  Logical; if `TRUE`, prints the weighted mean AGB and standard
  deviation. Defaults to `TRUE`.

## Value

A list containing:

- wm:

  Weighted mean of AGB (tons per hectare).

- wsd:

  Weighted mean of tree standard deviation.

- weighted_plots:

  The input data frame with weights assigned in a `wt` column.

## Examples

``` r
  # Example 1: Basic usage with synthetic data
  strata_plots <- data.frame(
    AGB_T_HA = c(100, 150, 200),
    sdTree = c(10, 15, 20),
    stratum = c("A", "B", "C")
  )
  result <- StrataAGB(strata_plots)
#> Plot AGB is 170.54 with SD 17.05 
  print(result$wm)  # Weighted mean AGB
#> [1] 170.5357
  print(result$wsd) # Weighted standard deviation
#> [1] 17.05357

  # Example 2: Custom weights and no printing
  custom_weights <- c("Low" = 0.3, "High" = 0.7)
  plots <- data.frame(
    AGB_T_HA = c(120, 180),
    sdTree = c(12, 18),
    stratum = c("Low", "High")
  )
  result <- StrataAGB(plots, strata_weights = custom_weights, verbose = FALSE)
  print(result$weighted_plots)
#>   AGB_T_HA sdTree stratum  wt
#> 1      120     12     Low 0.3
#> 2      180     18    High 0.7
```
