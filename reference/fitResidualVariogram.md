# Fit Variogram to Residuals After Bias Modeling

Fits a variogram model to the scaled residuals from bias modeling to
characterize spatial autocorrelation in the remaining prediction errors.
This is a critical step for uncertainty quantification as it allows
proper aggregation of standard errors accounting for spatial
correlation.

## Usage

``` r
fitResidualVariogram(
  bias_data,
  map_sd_raster,
  model = "Sph",
  cutoff = 50,
  width = cutoff/15,
  psill = 3,
  range = 10,
  nugget = 4,
  coord_cols = c("lon", "lat"),
  crs = "EPSG:4326",
  remove_outliers = TRUE,
  plot = TRUE
)
```

## Arguments

- bias_data:

  A data.frame with plot locations and bias modeling results. Must
  contain columns: `bias` (observed bias), `biasPred` (predicted bias
  from
  [`trainBiasModel()`](https://atnt.github.io/Plot2Map/reference/trainBiasModel.md)),
  coordinates (lon/lat or x/y), and will be joined with SD values from
  `map_sd_raster`.

- map_sd_raster:

  SpatRaster containing pixel-level standard deviation of AGB (Mg/ha).
  Used to scale residuals: resid = (bias - biasPred) / sd.

- model:

  Character. Variogram model type to fit. Options: "Sph" (Spherical,
  default), "Exp" (Exponential), "Gau" (Gaussian), "Mat" (Matern). See
  [`gstat::vgm()`](https://r-spatial.github.io/gstat/reference/vgm.html)
  for details.

- cutoff:

  Numeric. Maximum distance in kilometers for variogram calculation.
  Default: 50. Should be roughly half the maximum distance between
  plots. The function automatically converts to meters after UTM
  transformation.

- width:

  Numeric. Width of distance bins in kilometers for empirical variogram.
  Default: `cutoff/15`. Smaller values give more detail but more noise.

- psill:

  Numeric. Initial partial sill for variogram fitting. Default: 3.
  Adjust based on variance of scaled residuals.

- range:

  Numeric. Initial range parameter in kilometers. Default: 10.
  Represents distance at which correlation becomes negligible.

- nugget:

  Numeric. Initial nugget (y-intercept) for variogram. Default: 4.
  Represents measurement error and micro-scale variation.

- coord_cols:

  Character vector of length 2. Column names for coordinates in
  `bias_data`. Default: `c("lon", "lat")`. Use `c("x", "y")` if data are
  already projected.

- crs:

  Character or numeric. Coordinate reference system for `bias_data`.
  Default: "EPSG:4326" (WGS84). Must match `map_sd_raster` CRS or be
  transformable to it.

- remove_outliers:

  Logical. Should extreme residuals (\|resid\| \> 3) be removed before
  variogram fitting? Default: TRUE. Helps prevent fitting issues.

- plot:

  Logical. Should a diagnostic plot be generated? Default: TRUE. Shows
  empirical variogram points and fitted model curve.

## Value

A list with class "residualVariogram" containing:

- variogram_model:

  gstat vgm object with fitted variogram parameters

- empirical_variogram:

  gstat variogram object with empirical semivariances

- residuals:

  data.frame with columns: coordinates, bias, biasPred, sd, resid0 (raw
  residuals), resid (scaled residuals)

- resid_sf:

  sf object with residuals (used for variogram fitting)

- fit_quality:

  data.frame with fitting diagnostics: SSErr (sum of squared errors),
  RMSE, R2 (pseudo-R²)

- n_obs:

  Integer. Number of observations used in fitting

- n_removed:

  Integer. Number of outliers removed (if remove_outliers=TRUE)

- plot:

  ggplot object if plot=TRUE, NULL otherwise

## Details

This function implements a residual variogram fitting approach with the
following workflow:

1.  Extract SD values at plot locations from `map_sd_raster`

2.  Calculate raw residuals: resid0 = bias - biasPred

3.  Scale residuals by map SD: resid = resid0 / sd

4.  Optionally remove outliers (\|resid\| \> 3)

5.  Convert to sf object for gstat

6.  Fit empirical variogram with
    [`gstat::variogram()`](https://r-spatial.github.io/gstat/reference/variogram.html)

7.  Fit theoretical variogram model with
    [`gstat::fit.variogram()`](https://r-spatial.github.io/gstat/reference/fit.variogram.html)

**Scaled residuals:** Scaling by SD is critical because it standardizes
residuals to unitless values, allowing variogram fitting across regions
with different absolute error magnitudes. The fitted variogram will be
used to create correlation matrices for uncertainty aggregation.

**Model selection:** The default "Sph" (Spherical) model is appropriate
for most ecological data as it reaches a finite sill at a defined range.
Use "Exp" (Exponential) if correlation decays more gradually, or "Gau"
(Gaussian) for very smooth spatial processes.

**Initial parameters:** The psill, range, and nugget parameters are
starting values for nonlinear optimization. Poor initial values can lead
to convergence issues. Inspect the empirical variogram first to choose
reasonable starting values.

## References

Araza, A., de Bruin, S., Herold, M., Quegan, S., Labrière, N.,
Rodriguez-Veiga, P., ... & Burt, A. (2022). A comprehensive framework
for assessing the accuracy and uncertainty of global above-ground
biomass maps. Remote Sensing of Environment, 272, 112917.

Christensen, W. F. (2011). Filtered kriging for spatial data with
heterogeneous measurement error variances. Biometrics, 67(3), 947-957.

## See also

[`trainBiasModel`](https://atnt.github.io/Plot2Map/reference/trainBiasModel.md)
for bias model training,
[`discountVariogram`](https://atnt.github.io/Plot2Map/reference/discountVariogram.md)
for variogram nugget adjustment,
[`aggregateUncertainty`](https://atnt.github.io/Plot2Map/reference/aggregateUncertainty.md)
for spatial uncertainty aggregation

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming you've run bias modeling workflow:
# 1. Extract covariates and calculate bias
bias_data <- extractBiasCovariates(
  plot_data = plots,
  map_agb = agb_map,
  map_sd = sd_map,
  covariates = list(height = height_raster, treecover = tc_raster)
)

# 2. Train bias model
bias_model <- trainBiasModel(
  bias_data = bias_data,
  predictors = c("map", "sd", "height", "treecover"),
  cv_folds = 10
)

# 3. Add predictions to bias_data
bias_data$biasPred <- predict(bias_model$model, data = bias_data)

# 4. Fit residual variogram
vgm_fit <- fitResidualVariogram(
  bias_data = bias_data,
  map_sd_raster = sd_map,
  model = "Sph",
  cutoff = 50,
  psill = 3,
  range = 10,
  nugget = 4,
  coord_cols = c("x", "y"),
  crs = "EPSG:4326"
)

# Inspect results
print(vgm_fit$variogram_model)
print(vgm_fit$fit_quality)
print(vgm_fit$plot)

# Check residual distribution
hist(vgm_fit$residuals$resid, main = "Scaled Residuals")
summary(vgm_fit$residuals$resid)
} # }
```
