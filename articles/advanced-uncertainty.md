# Advanced Uncertainty Quantification with Spatial Correlation

## Introduction

This vignette continues from
[`vignette("bias-modeling")`](https://atnt.github.io/Plot2Map/articles/bias-modeling.md)
and demonstrates how to properly quantify uncertainty in regional
biomass estimates. The workflow follows the geostatistical approach from
[Araza et al. (2022)](https://doi.org/10.1016/j.rse.2022.112917) and the
Mexico demo notebook.

The key insight is that **assuming pixel independence underestimates
uncertainty** because spatial correlation is ignored.

### Prerequisites

Complete the bias modeling workflow first. You should have these
objects:

``` r
# From bias-modeling.Rmd:
# - AGBdata01: aggregated plot data from invDasymetry() WITH varPlot column
#              (requires running Step 1.3: calculateTotalUncertainty BEFORE invDasymetry)
# - bias_data: data.frame with plot-level bias and covariates (x, y, bias, map, sd, ...)
# - bias_model: trained Random Forest model from trainBiasModel()
# - agb_map_aligned: original AGB map (aligned to covariates)
# - sd_map: pixel-level uncertainty map (SD in Mg/ha)
# - map_adjusted: bias-corrected AGB map from adjustMapBias()
# - covariates_stack: environmental covariates from spatialcovariates

# IMPORTANT: AGBdata01 must have varPlot column from Step 1.3 in bias-modeling.Rmd
# If varPlot is missing, go back and run Step 1.3: calculateTotalUncertainty() before invDasymetry()
```

### Why Spatial Correlation Matters

**Naive approach (independence assumption):**
\\\text{SD}\_{\text{independent}} = \sqrt{\sum_i \text{SD}\_i^2}\\

**Correct approach (with spatial correlation):**
\\\text{SD}\_{\text{correlated}} = \sqrt{\sum_i \sum_j \text{SD}\_i
\times \text{SD}\_j \times \rho\_{ij}}\\

where \\\rho\_{ij}\\ is the spatial correlation between pixels derived
from a variogram model.

In the Mexico demo, accounting for spatial correlation increased the
uncertainty estimate from 31.6 Pg to 277.9 Pg - nearly 9x larger!

## Step 1: Prepare Residuals for Variogram Fitting

First, add model predictions to `bias_data` and calculate scaled
residuals:

``` r
# Add predictions to bias_data
if (bias_model$method == "ranger") {
  bias_data$biasPred <- predict(bias_model$model, data = bias_data)$predictions
} else {
  bias_data$biasPred <- predict(bias_model$model, newdata = bias_data)
}

# Calculate raw residuals (bias - predicted bias)
bias_data$resid0 <- bias_data$bias - bias_data$biasPred

# Scale residuals by map SD (critical step!)
# This standardizes residuals across regions with different error magnitudes
bias_data$resid <- bias_data$resid0 / bias_data$sd

# Remove infinite values (from zero SD)
bias_data <- subset(bias_data, is.finite(resid))

# Check distribution
hist(bias_data$resid, main = "Scaled AGB Residuals", xlab = "Scaled residual")
```

**Why scale by SD?** Scaling standardizes residuals to unitless values,
allowing variogram fitting across regions with different absolute error
magnitudes. The fitted variogram describes the spatial correlation
structure of *relative* errors.

## Step 2: Fit Residual Variogram

Fit a variogram to the scaled residuals to characterize spatial
autocorrelation.

**Important**: Reliable variogram fitting requires: - At least 30-50
plots (more is better) - Good spatial distribution across the study
area - Sufficient distance range between plots

With fewer plots, variogram fitting may be unstable. The Spain example
in the bias-modeling vignette provides ~500 aggregated grid cells, which
is sufficient for reliable variogram estimation.

``` r
library(Plot2Map)

# First, examine your scaled residuals to choose appropriate initial parameters
cat("Scaled residual variance:", var(bias_data$resid, na.rm = TRUE), "\n")
cat("Scaled residual range:", range(bias_data$resid, na.rm = TRUE), "\n")
# Scaled residual variance: 97.76873 
# Scaled residual range: -82.82981 449.4687 

# IMPORTANT: Initial psill and nugget should be based on the VARIANCE of scaled residuals
# If var(resid) ≈ 0.5, use psill ≈ 0.3, nugget ≈ 0.2
# If var(resid) ≈ 1.0, use psill ≈ 0.6, nugget ≈ 0.4

# Calculate suggested initial values
resid_var <- var(bias_data$resid, na.rm = TRUE)
suggested_psill <- resid_var * 0.6
suggested_nugget <- resid_var * 0.4

cat("Suggested initial psill:", round(suggested_psill, 3), "\n")
cat("Suggested initial nugget:", round(suggested_nugget, 3), "\n")
# Suggested initial psill: 58.661 
# Suggested initial nugget: 39.107 

# Fit variogram to scaled residuals
# Note: fitResidualVariogram() transforms geographic coords to UTM for accurate
# distance calculations. All distance parameters (cutoff, width, range) are in km.
vgm_fit <- fitResidualVariogram(
  bias_data = bias_data,
  map_sd_raster = sd_map,
  model = "Sph",           # Spherical model
  cutoff = 500,            # Maximum distance in km (adjust to ~half study extent)
  width = 33,              # Bin width in km (cutoff / 15)
  psill = suggested_psill, # Initial partial sill based on residual variance
  range = 100,             # Initial range in km
  nugget = suggested_nugget, # Initial nugget based on residual variance
  coord_cols = c("x", "y"),
  crs = "EPSG:4326",
  remove_outliers = TRUE,
  plot = TRUE
)

# View fitted model
print(vgm_fit$variogram_model)

# Examine fit quality
# R² should be positive; negative R² indicates poor fit - adjust parameters
print(vgm_fit$fit_quality)

#   model     psill  range
# 1   Nug 0.5565663      0
# 2   Sph 0.2164059 154476
#         SSErr       RMSE        R2
# 1 0.006368057 0.01995003 0.8295572
```

### Understanding Variogram Parameters

- **Nugget**: Variance at zero distance (measurement error + micro-scale
  variation)
- **Partial sill (psill)**: Additional variance explained by spatial
  structure
- **Total sill**: Nugget + psill (total variance at large distances)
- **Range**: Distance at which correlation becomes negligible (varies by
  study area)

### Interpreting the Variogram

``` r
vgm <- vgm_fit$variogram_model

# Extract parameters (range is in meters after UTM conversion)
nugget_val <- vgm$psill[1]
sill_val <- sum(vgm$psill)
range_val <- vgm$range[2] / 1000  # Convert meters to km

# Nugget-to-sill ratio
nugget_ratio <- nugget_val / sill_val
cat("Nugget-to-sill ratio:", round(nugget_ratio, 2), "\n")
# < 0.25: Strong spatial structure
# > 0.75: Weak spatial structure (mostly random noise)

cat("Effective range:", round(range_val, 1), "km\n")

# Nugget-to-sill ratio: 0.72 
# Effective range: 154.5 km
```

## Step 3: Discount Variogram for Plot Measurement Error

The variogram nugget includes both map error and plot measurement error.
We discount the nugget to isolate the map error component:

``` r
# Merge varPlot from AGBdata01 into bias_data
# invDasymetry already calculated varPlot when we used aggr = 0.1
bias_data$varPlot <- AGBdata01$varPlot[
  match(paste(bias_data$x, bias_data$y),
        paste(AGBdata01$x, AGBdata01$y))
]

# Calculate varMap (map pixel variance)
bias_data$varMap <- bias_data$sd^2

# Verify required columns
cat("Required columns present:",
    all(c("varPlot", "varMap") %in% names(bias_data)), "\n")
cat("Non-NA varPlot values:", sum(!is.na(bias_data$varPlot)), "of", nrow(bias_data), "\n")

# Discount variogram
vgm_discounted <- discountVariogram(
  variogram_fit = vgm_fit,
  plot_data = bias_data,
  filter_iqr = TRUE,       # IQR filtering (25-75th percentile)
  iqr_mult = 1.5,
  transfer_negative = TRUE,
  plot = FALSE  # Don't plot to screen, will save to PNG
)

# View discount factor
cat("Discount factor:", round(vgm_discounted$discount_factor, 4), "\n")
cat("Original nugget:", round(vgm_discounted$nugget_original, 3), "\n")
cat("Discounted nugget:", round(vgm_discounted$nugget_discounted, 3), "\n")

# Compare variograms
print(vgm_discounted$original_variogram)
print(vgm_discounted$discounted_variogram)

# Discount factor: 0.0848
# Original nugget: 0.557
# Discounted nugget: 0.472
#   model     psill  range
# 1   Nug 0.5565663      0
# 2   Sph 0.2164059 154476
#   model     psill  range
# 1   Nug 0.4717966      0
# 2   Sph 0.2164059 154476

# Plot comparison
maxdist_m <- max(vgm_fit$empirical_variogram$dist)
vgm_orig_fitted <- gstat::variogramLine(vgm_discounted$original_variogram,
                                        maxdist = maxdist_m)
vgm_disc_fitted <- gstat::variogramLine(vgm_discounted$discounted_variogram,
                                        maxdist = maxdist_m)

plot(vgm_fit$empirical_variogram$dist / 1000,
     vgm_fit$empirical_variogram$gamma,
     pch = 20, cex = 1.5, col = "gray50",
     xlab = "Distance (km)", ylab = "Semivariance",
     main = "Original vs Discounted Variogram",
     ylim = c(0, max(vgm_fit$empirical_variogram$gamma, na.rm = TRUE)))
lines(vgm_disc_fitted$dist / 1000,
      vgm_disc_fitted$gamma,
      col = "blue", lwd = 3)
lines(vgm_orig_fitted$dist / 1000,
      vgm_orig_fitted$gamma,
      col = "red", lwd = 3, lty = 2)
legend("bottomright",
       legend = c("Empirical", "Original (before discount)", "Discounted (after discount)"),
       pch = c(20, NA, NA), lty = c(NA, 2, 1),
       col = c("gray50", "red", "blue"), lwd = c(NA, 3, 3))
```

![Original vs Discounted Variogram for Iberian
Peninsula](variogram_discounted_iberia.png)

Original vs Discounted Variogram for Iberian Peninsula

### Understanding Discounting

The discount factor is calculated as: `mean(varPlot / varMap²)`

This represents the proportion of the nugget attributable to plot
measurement error rather than map error. After discounting: - Large
discount (\> 0.5): Most nugget was plot error - Small discount (\< 0.2):
Most nugget was map error

## Step 4: Create Correlation Matrix

Build a correlation matrix from the discounted variogram for focal
aggregation:

``` r
# The aggregateUncertainty function creates this internally, but here's the logic:
# Correlation: rho(h) = 1 - gamma(h) / sill

# For a focal window at resolution 0.1° (~10 km):
# - Create distance grid within window
# - Convert distances to semivariances using variogram
# - Convert semivariances to correlations

# The matrix size depends on the variogram range
# For range ~8 km and 0.1° resolution, window covers ~2-3 cells in each direction
```

## Step 5: Aggregate Uncertainty with Spatial Correlation

Apply the discounted variogram to aggregate pixel-level uncertainty at
the regional scale:

``` r
# First, align sd_map with covariates_stack for consistent extents/resolution
sd_map_aligned <- terra::resample(sd_map, covariates_stack[["tcc2010"]],
                                   method = "bilinear")

# Aggregate uncertainty accounting for spatial correlation
# This uses focal window aggregation with correlation weights
uncertainty_agg <- aggregateUncertainty(
  sd_raster = sd_map_aligned,
  variogram_model = vgm_discounted,
  resolution = 0.1,        # Aggregation resolution in degrees (~10 km)
  forest_mask = covariates_stack[["tcc2010"]],
  return_comparison = TRUE,
  scale_factor = 10000,    # Convert 0.1° to hectares (100m pixels)
  verbose = TRUE
)

# Regional SD with spatial correlation
cat("Regional SD (with correlation):",
    format(uncertainty_agg$regional_sd_correlated, big.mark = ","), "Mg\n")

# Regional SD assuming independence
cat("Regional SD (independence):",
    format(uncertainty_agg$regional_sd_independent, big.mark = ","), "Mg\n")

# Correlation increases uncertainty by this factor:
correlation_ratio <- uncertainty_agg$regional_sd_correlated / uncertainty_agg$regional_sd_independent
cat("Correlation ratio:", round(correlation_ratio, 2), "x\n")
cat("Spatial correlation increases uncertainty by",
    round((correlation_ratio - 1) * 100, 1), "%\n")

# Regional SD (with correlation): 57,993,482 Mg
# Regional SD (independence): 43,911,980 Mg
# Correlation ratio: 1.32 x
# Spatial correlation increases uncertainty by 32.1 %

# Visualize aggregated SD
plot(uncertainty_agg$aggregated_sd_raster,
     main = "Aggregated SD (Mg) - With Spatial Correlation",
     col = terrain.colors(50))
```

![Aggregated SD (Mg) - With Spatial Correlation for Iberian
Peninsula](agg_spa_corr_iberia.png)

Aggregated SD (Mg) - With Spatial Correlation for Iberian Peninsula

### How Aggregation Works

The following approach is used:

1.  **Create correlation matrix** from variogram using Haversine
    distances
2.  **Apply focal aggregation**: For each pixel, calculate weighted sum
    of SD products: \\\text{sumErr} = \sum_i \text{SD}\_{\text{center}}
    \times \text{SD}\_i \times \rho_i\\
3.  **Regional total**: \\\text{SD}\_{\text{total}} = \sqrt{\sum
    \text{sumErr}} \times \text{scale\\factor}\\

The scale factor (10000) converts from 0.1° grid cells to hectares based
on 100m pixel resolution.

### Key Findings

The 32% increase in uncertainty when accounting for spatial correlation
is substantial and demonstrates that:

1.  **Map errors are spatially correlated** at distances up to ~155 km
    (the variogram range)
2.  **The independence assumption is invalid** - treating pixels as
    independent underestimates uncertainty
3.  **Proper uncertainty quantification requires geostatistical
    methods** that account for spatial structure

This result aligns with findings from Araza et al. (2022), who showed
that ignoring spatial correlation can lead to severe underestimation of
regional biomass uncertainty. While our 32% increase is more moderate
than the Mexico demo’s 9x factor, it reflects:

- **Different spatial scales**: Spain (Iberian Peninsula) is smaller
  than Mexico
- **Different forest types**: Temperate forests have different error
  structures than tropical
- **Moderate spatial correlation**: Our nugget-to-sill ratio of 0.72
  indicates weaker spatial structure than Mexico

### Implications for Carbon Reporting

For national greenhouse gas inventories and REDD+ MRV systems, these
results show that:

- Pixel-based uncertainty estimates must be propagated with spatial
  correlation
- Simply summing pixel-level SDs in quadrature will underestimate
  regional uncertainty
- The variogram-based approach provides defensible uncertainty bounds
  for policy decisions

## Step 6: Calculate Regional Totals with Confidence Intervals

Combine everything to report bias-adjusted totals with proper
uncertainty:

``` r
# Calculate regional totals for both default and adjusted maps
regional_totals <- biasAdjustedTotal(
  map_agb_default = agb_map_aligned,
  map_agb_adjusted = map_adjusted,
  aggregated_uncertainty = uncertainty_agg,
  forest_mask = covariates_stack[["tcc2010"]],
  threshold = 10,
  confidence_level = 0.95,
  scale_factor = 10000,
  return_details = TRUE
)

# View results
print(regional_totals)

# Check if bias correction is statistically significant
overlap <- attr(regional_totals, "uncertainty_overlap")
if (!overlap) {
  cat("\nConfidence intervals do NOT overlap - bias correction is statistically significant\n")
} else {
  cat("\nConfidence intervals overlap - bias correction is NOT statistically significant\n")
}

# Bias correction effect
cat("\nBias correction effect:",
    format(attr(regional_totals, "bias_correction_effect"), big.mark = ","), "Mg\n")
cat("Percent change:", round(attr(regional_totals, "percent_change"), 2), "%\n")

#        map_type   total_Mg    sd_Mg cv_percent   ci_lower   ci_upper confidence_level n_cells mean_agb_Mg_ha
# 1       default 2275748993 57993482   2.548325 2162083858 2389414128             0.95    4776       47.64969
# 2 bias_adjusted 2411995142 57993482   2.404378 2298330006 2525660277             0.95    4776       50.50241
#   sd_mean_Mg_ha
# 1      1.214269
# 2      1.214269

# Confidence intervals overlap - bias correction is NOT statistically significant

# Bias correction effect: 136,246,149 Mg
# Percent change: 5.99 %
```

### Interpreting the Results

**Regional Biomass Estimates for Iberian Peninsula:**

- **Default map**: 2.28 Pg (95% CI: 2.16 - 2.39 Pg), CV = 2.5%
- **Bias-adjusted map**: 2.41 Pg (95% CI: 2.30 - 2.53 Pg), CV = 2.4%
- **Bias correction effect**: +136 Tg (6% increase)

**Statistical Significance:**

The overlapping confidence intervals indicate that the bias correction
is not statistically significant at the 95% confidence level. While the
bias modeling detected systematic errors and increased the estimate by
6%, the large uncertainty (±58 Tg) means we cannot conclude with 95%
confidence that the corrected map differs from the original.

**Why is the CV relatively low (~2.5%)?**

The Iberian Peninsula shows lower relative uncertainty than many
tropical regions because:

1.  **Excellent plot coverage**: 5,328 field plots well-distributed
    across the region
2.  **Homogeneous temperate forests**: Less structural complexity than
    tropical forests
3.  **Consistent map performance**: ESA CCI performs well in temperate
    regions
4.  **Moderate spatial correlation**: Nugget-to-sill ratio of 0.72
    indicates weaker spatial structure

## Common Issues and Solutions

### Issue: “Singular model in variogram fit” Warning

**Cause**: Initial parameters (psill, nugget) don’t match the scale of
your data

**Solution**: Base initial parameters on residual variance:

``` r
resid_var <- var(bias_data$resid, na.rm = TRUE)
# Use psill = resid_var * 0.6, nugget = resid_var * 0.4
```

### Issue: Negative R² in Fit Quality

**Cause**: Poor variogram fit - the model is worse than the mean

**Solutions**: 1. Adjust cutoff to match your study extent (~half the
maximum distance) 2. Use data-driven initial psill/nugget (see above) 3.
Try different model types (“Exp”, “Gau” instead of “Sph”) 4. Check if
you have enough spatial spread in your plots

### Issue: Variogram Fitting Fails Completely

**Possible causes**: - Too few data points (need 30+ plots for reliable
variograms) - Plots clustered in one area (insufficient spatial
spread) - Coordinates in wrong units or CRS

**Solution**: Check empirical variogram first, ensure good spatial
coverage

### Issue: Negative Nugget After Discounting

**Solution**: Use `transfer_negative = TRUE` to redistribute to partial
sill

### Issue: Very Large Correlation Ratio (\> 10)

This is expected when spatial correlation is strong. The Mexico demo
showed a ratio of ~8.8x. High ratios indicate: - Strong spatial
autocorrelation in map errors - Independence assumption would severely
underestimate uncertainty

## References

- Araza, A., et al. (2022). “A comprehensive framework for assessing the
  accuracy and uncertainty of global above-ground biomass maps.” *Remote
  Sensing of Environment*, 272, 112917.

- Christensen, W.F. (2011). “Filtered kriging for spatial data with
  heterogeneous measurement error variances.” *Biometrics*, 67(3),
  947-957.
