# Calculate Bias-Adjusted Regional Totals with Uncertainty

Calculates regional AGB totals for both default and bias-adjusted maps,
including uncertainty bounds accounting for spatial correlation.
Provides a complete comparison showing the impact of bias correction and
proper uncertainty quantification.

## Usage

``` r
biasAdjustedTotal(
  map_agb_default,
  map_agb_adjusted,
  aggregated_uncertainty,
  forest_mask = NULL,
  threshold = 10,
  confidence_level = 0.95,
  scale_factor = 10000,
  return_details = TRUE
)
```

## Arguments

- map_agb_default:

  SpatRaster containing the default (uncorrected) AGB map (Mg/ha).

- map_agb_adjusted:

  SpatRaster containing the bias-adjusted AGB map (Mg/ha). Typically
  from
  [`adjustMapBias()`](https://atnt.github.io/Plot2Map/reference/adjustMapBias.md).

- aggregated_uncertainty:

  An aggregatedUncertainty object from
  [`aggregateUncertainty()`](https://atnt.github.io/Plot2Map/reference/aggregateUncertainty.md),
  containing the aggregated SD raster and regional SD values.
  Alternatively, a SpatRaster with aggregated SD values.

- forest_mask:

  Optional SpatRaster with forest/non-forest mask (typically tree cover
  percentage). Used together with `threshold` to define forest areas.
  Default: NULL (all pixels used).

- threshold:

  Numeric value (0-100) specifying the minimum tree cover percentage to
  be considered forest. Only used if `forest_mask` is provided. Default:
  10 (areas with \<10% tree cover are excluded). Set to 0 to include all
  areas.

- confidence_level:

  Numeric. Confidence level for intervals. Default: 0.95 (95% confidence
  intervals). Common values: 0.90, 0.95, 0.99.

- scale_factor:

  Numeric. Conversion factor from pixel values to regional totals.
  Default: 10000 (converts 100m pixels to hectares for 0.1°
  aggregation). Must match the scale_factor used in
  [`aggregateUncertainty()`](https://atnt.github.io/Plot2Map/reference/aggregateUncertainty.md).

- return_details:

  Logical. Should detailed statistics be included? Default: TRUE.
  Includes CV%, relative uncertainty, number of cells, etc.

## Value

A data.frame with one row per map type (default, bias_adjusted) and
columns:

- map_type:

  Character. "default" or "bias_adjusted"

- total_Mg:

  Numeric. Regional AGB total (Mg)

- sd_Mg:

  Numeric. Standard deviation of total (Mg)

- cv_percent:

  Numeric. Coefficient of variation (SD/mean \* 100)

- ci_lower:

  Numeric. Lower confidence bound (Mg)

- ci_upper:

  Numeric. Upper confidence bound (Mg)

- confidence_level:

  Numeric. Confidence level used (e.g., 0.95)

- n_cells:

  Integer. Number of pixels included

- mean_agb_Mg_ha:

  Numeric. Mean AGB per hectare (only if return_details=TRUE)

- sd_mean_Mg_ha:

  Numeric. SD of mean per hectare (only if return_details=TRUE)

Additional attributes:

- bias_correction_effect:

  Numeric. Difference in totals (adjusted - default) in Mg

- percent_change:

  Numeric. Percent change from default to adjusted

- uncertainty_overlap:

  Logical. Do confidence intervals overlap?

## Details

This function integrates the complete bias modeling and uncertainty
quantification workflow from Sections 4-5 of the Mexico demo notebook
(lines 1079-1305).

**Regional totals calculation:**

For each map (default and bias-adjusted):

1.  Apply forest mask and threshold

2.  Sum all pixel values and multiply by scale_factor

3.  Extract or calculate regional SD

4.  Calculate confidence intervals: total ± (z \* SD)

**Confidence intervals:**

Confidence intervals are calculated using the normal approximation:

\$\$CI = total \pm z\_{\alpha/2} \times SD\$\$

where z is the critical value from the standard normal distribution
(e.g., 1.96 for 95% confidence).

**Coefficient of variation (CV%):**

CV is a normalized measure of uncertainty:

\$\$CV = (SD / mean) \times 100\$\$

Values \< 10% indicate low uncertainty, 10-30% moderate, \> 30% high.

**Interpretation:**

Compare the two rows to assess:

- **Bias correction impact:** How much did the total change?

- **Statistical significance:** Do confidence intervals overlap?

- **Relative uncertainty:** Is CV% acceptable for reporting?

- **IPCC compliance:** Are uncertainty bounds suitable for inventory?

## References

Araza, A., de Bruin, S., Herold, M., Quegan, S., Labrière, N.,
Rodriguez-Veiga, P., ... & Burt, A. (2022). A comprehensive framework
for assessing the accuracy and uncertainty of global above-ground
biomass maps. Remote Sensing of Environment, 272, 112917.

IPCC (2006). IPCC Guidelines for National Greenhouse Gas Inventories.

## See also

[`adjustMapBias`](https://atnt.github.io/Plot2Map/reference/adjustMapBias.md)
for bias correction,
[`aggregateUncertainty`](https://atnt.github.io/Plot2Map/reference/aggregateUncertainty.md)
for uncertainty aggregation

## Examples

``` r
if (FALSE) { # \dontrun{
# Complete workflow:
# 1. Bias modeling
bias_data <- extractBiasCovariates(plots, map_agb, map_sd, covariates)
bias_model <- trainBiasModel(bias_data, predictors = c("map", "sd", "height"))
bias_map <- predictBiasMap(bias_model, covariate_stack)
adjustment <- adjustMapBias(map_agb, bias_map, forest_mask)

# 2. Uncertainty quantification
bias_data$biasPred <- predict(bias_model$model, data = bias_data)
vgm_fit <- fitResidualVariogram(bias_data, map_sd, coord_cols = c("x", "y"), crs = "EPSG:4326")
vgm_disc <- discountVariogram(vgm_fit, variance_data)
uncertainty <- aggregateUncertainty(map_sd, vgm_disc, resolution = 0.1)

# 3. Calculate totals with uncertainty
totals <- biasAdjustedTotal(
  map_agb_default = map_agb,
  map_agb_adjusted = adjustment$adjusted_map,
  aggregated_uncertainty = uncertainty,
  forest_mask = forest_mask,
  confidence_level = 0.95
)

# Inspect results
print(totals)
print(attr(totals, "bias_correction_effect"))
print(attr(totals, "uncertainty_overlap"))
} # }
```
