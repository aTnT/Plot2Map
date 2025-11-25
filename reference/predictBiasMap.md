# Predict bias wall-to-wall across study region

This function applies a trained bias model to generate wall-to-wall
predictions of systematic bias across the study region. The predicted
bias can then be used to adjust the original AGB map.

## Usage

``` r
predictBiasMap(
  bias_model,
  covariate_stack,
  filename = NULL,
  return_raster = TRUE,
  ...
)
```

## Arguments

- bias_model:

  Trained model object from
  [`trainBiasModel`](https://atnt.github.io/Plot2Map/reference/trainBiasModel.md).
  Must be a list containing at minimum a 'model' component (ranger or
  randomForest object) and a 'predictors' component (character vector of
  predictor names).

- covariate_stack:

  SpatRaster object containing all predictor layers used in model
  training. Layer names must match the predictor names used in
  `bias_model$predictors`. Typically includes: map AGB, map SD, height,
  biome, tree cover, slope, aspect, and IFL.

- filename:

  Optional character string specifying output file path for the
  predicted bias raster. If provided, the raster will be written to
  disk. If NULL (default), the raster is kept in memory. For large
  regions, providing a filename is recommended to avoid memory issues.

- return_raster:

  Logical indicating whether to return the SpatRaster object. Default is
  TRUE. If FALSE and filename is provided, only the file path is
  returned.

- ...:

  Additional arguments passed to
  [`terra::writeRaster`](https://rspatial.github.io/terra/reference/writeRaster.html),
  such as overwrite, datatype, gdal options, etc.

## Value

If `return_raster = TRUE`, returns a SpatRaster object with predicted
bias values in Mg/ha. Positive values indicate areas where the map
overestimates AGB compared to expected values based on covariates.
Negative values indicate areas where the map underestimates AGB. If
`return_raster = FALSE` and `filename` is provided, returns the file
path as a character string.

## Details

This function uses
[`terra::predict`](https://rspatial.github.io/terra/reference/predict.html)
to apply the trained Random Forest model to a raster stack of
covariates. The function automatically handles both ranger and
randomForest model objects.

The covariate stack must have layer names that exactly match the
predictor variable names used during model training. Use
`names(covariate_stack)` to check layer names and
`names(covariate_stack) <- c(...)` to rename if needed.

For large regions or high-resolution rasters, it is recommended to:

- Provide a filename to write directly to disk

- Use appropriate datatype (e.g., datatype = "FLT4S" for 4-byte floats)

- Consider tiling options via GDAL options

The predicted bias map will have:

- Same extent, resolution, and CRS as the input covariate_stack

- NA values where any covariate has NA

- Values typically ranging from -100 to +100 Mg/ha for most forests

## References

Araza, A., et al. (2022). A comprehensive framework for assessing the
accuracy and uncertainty of global above-ground biomass maps. Remote
Sensing of Environment, 272, 112917.
https://doi.org/10.1016/j.rse.2022.112917

## See also

[`trainBiasModel`](https://atnt.github.io/Plot2Map/reference/trainBiasModel.md)
for training the bias model,
[`adjustMapBias`](https://atnt.github.io/Plot2Map/reference/adjustMapBias.md)
for applying the predicted bias to adjust the AGB map,
[`extractBiasCovariates`](https://atnt.github.io/Plot2Map/reference/extractBiasCovariates.md)
for the initial data preparation

## Examples

``` r
if (FALSE) { # \dontrun{
# Following from trainBiasModel example
# Assuming bias_model is already created

# Create covariate stack matching model predictors
library(terra)
r <- rast(ncol = 100, nrow = 100, xmin = -10, xmax = 10, ymin = 35, ymax = 45)
map_layer <- setValues(r, runif(ncell(r), 0, 300))
sd_layer <- setValues(r, runif(ncell(r), 10, 50))
height_layer <- setValues(r, runif(ncell(r), 5, 30))
treecover_layer <- setValues(r, runif(ncell(r), 0, 100))

# Stack covariates with correct names
covs <- c(map_layer, sd_layer, height_layer, treecover_layer)
names(covs) <- c("map", "sd", "height", "treecover")

# Predict bias across region
bias_map <- predictBiasMap(
  bias_model = bias_model,
  covariate_stack = covs
)

# Plot results
plot(bias_map, main = "Predicted Bias (Mg/ha)")

# Save to file
bias_map <- predictBiasMap(
  bias_model = bias_model,
  covariate_stack = covs,
  filename = "predicted_bias.tif",
  overwrite = TRUE
)
} # }
```
