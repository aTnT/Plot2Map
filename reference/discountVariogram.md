# Discount Variogram Nugget for Plot Measurement Error

Adjusts the variogram nugget to account for plot-level measurement
uncertainty using the method of Christensen (2011). This is essential
for proper uncertainty quantification as it separates spatial variation
(which aggregates differently) from measurement error (which does not).

## Usage

``` r
discountVariogram(
  variogram_fit,
  plot_data,
  filter_iqr = TRUE,
  iqr_mult = 1.5,
  transfer_negative = TRUE,
  plot = TRUE
)
```

## Arguments

- variogram_fit:

  A residualVariogram object from
  [`fitResidualVariogram()`](https://atnt.github.io/Plot2Map/reference/fitResidualVariogram.md),
  or a gstat vgm object to be discounted.

- plot_data:

  A data.frame with plot-level variance estimates. Must contain columns:
  `varPlot` (plot measurement variance in (Mg/ha)²) and `varMap` (map
  pixel variance in (Mg/ha)²). Typically obtained from
  [`calculateTotalUncertainty()`](https://atnt.github.io/Plot2Map/reference/calculateTotalUncertainty.md)
  or similar.

- filter_iqr:

  Logical. Should plots be filtered by IQR to remove extreme variance
  ratios? Default: TRUE. Christensen (2011) recommends this to avoid
  bias from outliers.

- iqr_mult:

  Numeric. IQR multiplier for outlier detection. Default: 1.5. Plots
  with varPlot/varMap² outside the range
  `[Q1 - iqr_mult * IQR, Q3 + iqr_mult * IQR]` are removed.

- transfer_negative:

  Logical. Should negative nugget values be transferred to the partial
  sill? Default: TRUE. Prevents invalid variogram models when
  discounting reduces nugget below zero.

- plot:

  Logical. Should a comparison plot be generated? Default: TRUE. Shows
  original vs. discounted variogram models.

## Value

A list with class "discountedVariogram" containing:

- discounted_variogram:

  gstat vgm object with adjusted nugget

- original_variogram:

  gstat vgm object before discounting

- discount_factor:

  Numeric. The discount value subtracted from nugget: mean(varPlot /
  varMap²)

- nugget_original:

  Numeric. Nugget before discounting

- nugget_discounted:

  Numeric. Nugget after discounting (≥ 0)

- nugget_transferred:

  Numeric. Amount transferred to partial sill if nugget would have been
  negative

- n_plots:

  Integer. Number of plots used in calculation

- n_filtered:

  Integer. Number of plots removed by IQR filtering

- plot:

  Function to generate comparison plot if plot=TRUE, NULL otherwise

## Details

This function implements the variogram discounting method from
Christensen (2011) as applied in Section 5 of the Mexico demo notebook
(lines 1177-1213).

**Christensen (2011) method:**

The variogram nugget includes both true micro-scale spatial variation
and plot measurement error. To isolate spatial variation, we calculate:

\$\$discount = mean(varPlot / varMap^2)\$\$

And adjust the nugget:

\$\$nugget\_{new} = nugget\_{old} - discount\$\$

**IQR filtering:** The variance ratio varPlot/varMap² can have extreme
values for plots with very low map variance or unusually high plot
variance. Christensen (2011) recommends filtering by the interquartile
range (IQR) to remove these outliers before calculating the mean
discount factor.

**Negative nugget handling:** If discounting results in a negative
nugget, the nugget is set to zero and the negative amount is added to
the partial sill. This ensures the variogram model remains valid while
preserving total sill.

**Interpretation:** A large discount factor indicates that plot
measurement error is a substantial component of the original nugget.
After discounting, the remaining nugget represents true micro-scale
spatial variation plus any remaining measurement error components.

## References

Christensen, W. F. (2011). Filtered kriging for spatial data with
heterogeneous measurement error variances. Biometrics, 67(3), 947-957.

## See also

[`fitResidualVariogram`](https://atnt.github.io/Plot2Map/reference/fitResidualVariogram.md)
for variogram fitting,
[`aggregateUncertainty`](https://atnt.github.io/Plot2Map/reference/aggregateUncertainty.md)
for using discounted variogram in aggregation

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming you have a fitted variogram and plot variance data:
vgm_fit <- fitResidualVariogram(
  bias_data = bias_data,
  map_sd_raster = sd_map,
  coord_cols = c("x", "y"),
  crs = "EPSG:4326"
)

# Calculate plot and map variances (example)
variance_data <- data.frame(
  varPlot = bias_data$sdPlot^2,  # From field measurement uncertainty
  varMap = bias_data$sd^2         # From map pixel SD
)

# Discount the variogram
vgm_discounted <- discountVariogram(
  variogram_fit = vgm_fit,
  plot_data = variance_data,
  filter_iqr = TRUE,
  iqr_mult = 1.5
)

# Inspect results
print(vgm_discounted$discount_factor)
print(vgm_discounted$discounted_variogram)

# Show comparison plot
if (!is.null(vgm_discounted$plot)) {
  vgm_discounted$plot()
}
} # }
```
