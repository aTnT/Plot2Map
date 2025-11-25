# Adjust AGB map by removing predicted bias

This function adjusts an AGB map by subtracting the predicted bias,
optionally applies a forest mask, and calculates regional totals for
comparison. This allows to provide bias-corrected AGB estimates at both
pixel and regional scales.

## Usage

``` r
adjustMapBias(
  map_agb,
  bias_map,
  forest_mask = NULL,
  threshold = 10,
  scale_factor = NULL,
  return_totals = TRUE
)
```

## Arguments

- map_agb:

  SpatRaster object containing the original AGB map to be adjusted.
  Values should be in Mg/ha. This is typically the same map used in
  earlier steps of the bias modeling workflow.

- bias_map:

  SpatRaster object containing predicted bias from
  [`predictBiasMap`](https://atnt.github.io/Plot2Map/reference/predictBiasMap.md).
  Must have the same extent, resolution, and CRS as `map_agb`. Values in
  Mg/ha.

- forest_mask:

  Optional SpatRaster object for forest masking, typically a tree cover
  layer. If provided, used together with `threshold` to define forest
  areas for regional total calculations. If NULL (default), no masking
  is applied.

- threshold:

  Numeric value (0-100) specifying the minimum tree cover percentage to
  be considered forest. Only used if `forest_mask` is provided. Default
  is 10 (i.e., areas with \<10% tree cover are excluded). Set to 0 to
  include all areas with any tree cover.

- scale_factor:

  Numeric value for scaling pixel-level values to regional totals.
  Typically calculated as (resolution_in_meters^2) / 10000 to convert
  from per-hectare to total. If NULL (default), automatically calculated
  from raster resolution assuming input resolution is in degrees.

- return_totals:

  Logical indicating whether to calculate regional totals. Default is
  TRUE. If FALSE, only the adjusted raster is returned without total
  calculations.

## Value

If `return_totals = TRUE`, returns a list with:

- adjusted_map:

  SpatRaster with bias-adjusted AGB values (Mg/ha). Calculated as:
  adjusted = original - predicted_bias

- default_total:

  Numeric, total AGB from original map in Mg (if return_totals=TRUE)

- adjusted_total:

  Numeric, total AGB from adjusted map in Mg (if return_totals=TRUE)

- difference:

  Numeric, difference between adjusted and default totals in Mg.
  Positive values indicate the adjusted estimate is higher.

- percent_change:

  Numeric, percentage change from default to adjusted. Formula: 100 \*
  (adjusted - default) / default

- n_cells:

  Integer, number of cells included in totals calculation (after
  masking)

If `return_totals = FALSE`, returns only the adjusted_map SpatRaster.

## Details

The bias adjustment follows a simple subtraction approach:
\$\$AGB\_{adjusted} = AGB\_{original} - bias\_{predicted}\$\$

Where `bias_predicted` is the output from
[`predictBiasMap`](https://atnt.github.io/Plot2Map/reference/predictBiasMap.md).
Areas where the model predicted positive bias (map overestimation) will
have their AGB reduced. Areas with negative bias (map underestimation)
will have their AGB increased.

Regional totals are calculated by summing all pixel values (after forest
masking) and multiplying by the scale factor to convert from per-hectare
to total biomass. The scale factor accounts for pixel size:

\$\$Total = \sum (AGB\_{pixel} \times PixelArea)\$\$

For example, with 0.1-degree pixels at the equator (~11km), the scale
factor would be approximately (11000^2) / 10000 = 12100.

## References

Araza, A., et al. (2022). A comprehensive framework for assessing the
accuracy and uncertainty of global above-ground biomass maps. Remote
Sensing of Environment, 272, 112917.
https://doi.org/10.1016/j.rse.2022.112917

## See also

[`predictBiasMap`](https://atnt.github.io/Plot2Map/reference/predictBiasMap.md)
for generating the bias predictions,
[`trainBiasModel`](https://atnt.github.io/Plot2Map/reference/trainBiasModel.md)
for training the bias model,
[`extractBiasCovariates`](https://atnt.github.io/Plot2Map/reference/extractBiasCovariates.md)
for the initial data preparation

## Examples

``` r
if (FALSE) { # \dontrun{
# Following from predictBiasMap example
# Assuming map_agb and bias_map are already created

# Simple adjustment without masking
adjusted <- adjustMapBias(
  map_agb = map_layer,
  bias_map = bias_map,
  return_totals = FALSE
)

plot(adjusted, main = "Bias-Adjusted AGB (Mg/ha)")

# Adjustment with forest mask and regional totals
result <- adjustMapBias(
  map_agb = map_layer,
  bias_map = bias_map,
  forest_mask = treecover_layer,
  threshold = 10,
  return_totals = TRUE
)

# Compare totals
print(result$default_total)
print(result$adjusted_total)
print(result$percent_change)

# Visualize difference
plot(result$adjusted_map - map_layer, main = "Change in AGB (Mg/ha)")
} # }
```
