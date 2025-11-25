# Aggregate Uncertainty Accounting for Spatial Correlation

Aggregates pixel-level standard deviation to regional scale while
accounting for spatial correlation using a variogram-based correlation
matrix. This is critical for accurate uncertainty quantification as
ignoring spatial correlation leads to underestimation of regional-scale
uncertainty.

## Usage

``` r
aggregateUncertainty(
  sd_raster,
  variogram_model,
  resolution = 0.1,
  forest_mask = NULL,
  filename = NULL,
  return_comparison = TRUE,
  scale_factor = 10000,
  verbose = TRUE
)
```

## Arguments

- sd_raster:

  SpatRaster containing pixel-level standard deviation of AGB (Mg/ha).
  Must have same extent and resolution as the AGB map.

- variogram_model:

  A discountedVariogram object from
  [`discountVariogram()`](https://atnt.github.io/Plot2Map/reference/discountVariogram.md),
  or a gstat vgm object containing the fitted (and optionally
  discounted) variogram parameters. Used to create the spatial
  correlation matrix.

- resolution:

  Numeric. Resolution (in degrees or CRS units) for aggregation.
  Default: 0.1 (roughly 10 km at equator). Defines the window size for
  focal aggregation. Should match the scale of regional reporting units.

- forest_mask:

  Optional SpatRaster with forest/non-forest mask. Only forest pixels
  (mask \> 0) will be included in aggregation. Default: NULL (all pixels
  used).

- filename:

  Optional. File path to save aggregated SD raster. Default: NULL
  (raster kept in memory).

- return_comparison:

  Logical. Should comparison statistics (with vs. without correlation)
  be calculated? Default: TRUE. Useful for assessing the impact of
  spatial correlation on regional uncertainty.

- scale_factor:

  Numeric. Conversion factor from pixel units to regional totals.
  Default: 10000 (converts 100m pixels to hectares when using 0.1°
  aggregation). Used to scale SD from mean per-pixel values to regional
  total values.

- verbose:

  Logical. Print progress messages? Default: TRUE.

## Value

A list with class "aggregatedUncertainty" containing:

- aggregated_sd_raster:

  SpatRaster with aggregated SD values accounting for spatial
  correlation (Mg)

- sd_no_correlation:

  SpatRaster with aggregated SD assuming independence (Mg). Only if
  return_comparison=TRUE

- correlation_matrix:

  Matrix used for focal window correlation weighting

- regional_sd_correlated:

  Numeric. Total regional SD accounting for correlation (Mg)

- regional_sd_independent:

  Numeric. Total regional SD assuming independence (Mg). Only if
  return_comparison=TRUE

- correlation_reduction_factor:

  Numeric. Ratio of correlated to independent SD. Values \< 1 indicate
  spatial correlation reduces total uncertainty. Only if
  return_comparison=TRUE

- n_cells:

  Integer. Number of cells included in aggregation

- resolution:

  Numeric. Aggregation resolution used

## Details

This function implements the focal window correlation approach from
Section 5 of the Mexico demo notebook (lines 1215-1305). The workflow
is:

1.  Create correlation matrix from variogram using `corMatrix()` helper

2.  Apply forest mask if provided

3.  Use
    [`terra::focal()`](https://rspatial.github.io/terra/reference/focal.html)
    with correlation matrix as weights

4.  Calculate weighted sum: sum(SD_i \* SD_j \* cor_ij)

5.  Take square root to get aggregated SD

6.  Scale to regional totals using scale_factor

**Spatial correlation and aggregation:**

When aggregating uncertainty from pixels to regions, we must account for
spatial correlation. The variance of a sum of correlated variables is:

\$\$Var(\sum X_i) = \sum_i \sum_j SD_i \times SD_j \times \rho\_{ij}\$\$

If correlation is ignored (independence assumption), this reduces to:

\$\$Var(\sum X_i) = \sum_i SD_i^2\$\$

Positive spatial correlation increases regional variance compared to the
independence assumption, because nearby pixels tend to have errors in
the same direction.

**Correlation matrix:**

The correlation matrix is created from the variogram using the helper
function `corMatrix()`, which converts semivariance γ(h) to correlation
ρ(h):

\$\$\rho(h) = 1 - \gamma(h) / sill\$\$

This matrix defines the correlation weights for the focal window
aggregation.

**Resolution and scale:**

The resolution parameter should match your reporting units (e.g., 0.1°
for regional estimates, 1° for country-level). The scale_factor converts
from pixel-level SD to regional total SD based on pixel size and
aggregation area.

## References

Araza, A., de Bruin, S., Herold, M., Quegan, S., Labrière, N.,
Rodriguez-Veiga, P., ... & Burt, A. (2022). A comprehensive framework
for assessing the accuracy and uncertainty of global above-ground
biomass maps. Remote Sensing of Environment, 272, 112917.

## See also

[`discountVariogram`](https://atnt.github.io/Plot2Map/reference/discountVariogram.md)
for variogram discounting,
[`biasAdjustedTotal`](https://atnt.github.io/Plot2Map/reference/biasAdjustedTotal.md)
for calculating regional totals with uncertainty

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming you have a discounted variogram:
vgm_discounted <- discountVariogram(vgm_fit, variance_data)

# Aggregate uncertainty with spatial correlation
uncertainty_agg <- aggregateUncertainty(
  sd_raster = sd_map,
  variogram_model = vgm_discounted,
  resolution = 0.1,
  forest_mask = forest_raster,
  return_comparison = TRUE
)

# Inspect results
print(uncertainty_agg$regional_sd_correlated)
print(uncertainty_agg$regional_sd_independent)
print(uncertainty_agg$correlation_reduction_factor)

# Plot aggregated SD
terra::plot(uncertainty_agg$aggregated_sd_raster,
            main = "Aggregated SD (with correlation)")
} # }
```
