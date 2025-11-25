# Extract bias and covariate values at plot locations

This function extracts covariate values at plot locations and calculates
bias as the difference between map AGB and plot AGB. This is the first
step in the bias modeling workflow, preparing data for training a bias
correction model. The function follows the methodology from Araza et al.
(2022) for AGB map bias assessment and correction.

## Usage

``` r
extractBiasCovariates(
  plot_data,
  map_agb,
  map_sd,
  covariates = list(),
  plot_agb_col = "plotAGB_10",
  map_agb_col = "mapAGB"
)
```

## Arguments

- plot_data:

  data.frame containing plot comparison results from
  [`invDasymetry`](https://atnt.github.io/Plot2Map/reference/invDasymetry.md).
  Must include columns:

  - x: X-coordinate (longitude) of plot locations

  - y: Y-coordinate (latitude) of plot locations

  - plotAGB_X: Plot AGB values where X is the threshold (e.g.,
    plotAGB_10)

  - mapAGB: Map AGB values extracted at plot locations

- map_agb:

  SpatRaster object containing the AGB map layer. This is typically the
  same map used in
  [`invDasymetry`](https://atnt.github.io/Plot2Map/reference/invDasymetry.md)
  for comparison. Values should be in Mg/ha.

- map_sd:

  SpatRaster object containing the standard deviation (uncertainty)
  layer associated with the AGB map. Values should be in Mg/ha.

- covariates:

  Named list of SpatRaster objects containing predictor variables.
  Common covariates include:

  - height: Canopy height (e.g., from Potapov et al. 2020)

  - biome: Biome classification (e.g., Dinerstein et al. 2017)

  - treecover: Tree cover percentage (e.g., Sexton et al. 2015)

  - slope: Terrain slope from DEM (e.g., SRTM)

  - aspect: Terrain aspect from DEM

  - ifl: Intact Forest Landscape indicator (binary)

  List names will be used as column names in the output. Set to NULL to
  skip covariate extraction.

- plot_agb_col:

  Character string specifying the column name for plot AGB in
  `plot_data`. Default is "plotAGB_10" (for 10% threshold).

- map_agb_col:

  Character string specifying the column name for map AGB in
  `plot_data`. Default is "mapAGB".

## Value

A data.frame containing all original columns from `plot_data` plus:

- bias:

  Calculated bias (mapAGB - plotAGB), in Mg/ha. Positive values indicate
  map overestimation, negative values indicate map underestimation.

- map:

  Map AGB value extracted at the plot location, in Mg/ha

- sd:

  Map SD value extracted at the plot location, in Mg/ha

- \<covariate_names\>:

  One column for each covariate in the covariates list. Column names
  match the list names. For the IFL covariate, NA values are
  automatically converted to 0.

## Details

This function implements the first step of the bias modeling workflow.
The bias is calculated as:

\$\$bias = AGB\_{map} - AGB\_{plot}\$\$

Positive bias values indicate that the map overestimates AGB compared to
plot measurements, while negative values indicate underestimation. The
extracted covariates will be used as predictors in the Random Forest
model to spatially model the bias patterns.

All raster extractions use bilinear interpolation by default. The
function ensures that all input rasters have matching coordinate
reference systems (CRS). Plots with NA values in any covariate will
retain the NA, which should be handled during model training.

## References

Araza, A., de Bruin, S., Herold, M., Quegan, S., Labriere, N.,
Rodriguez-Veiga, P., Avitabile, V., Santoro, M., Mitchard, E.T.A., Ryan,
C.M., Phillips, O.L., Willcock, S., Verbeeck, H., Carreiras, J., Hein,
L., Schelhaas, M.J., & Pacheco-Pascagaza, A.M. (2022). A comprehensive
framework for assessing the accuracy and uncertainty of global
above-ground biomass maps. Remote Sensing of Environment, 272, 112917.
https://doi.org/10.1016/j.rse.2022.112917

## See also

[`trainBiasModel`](https://atnt.github.io/Plot2Map/reference/trainBiasModel.md)
for training the bias correction model,
[`predictBiasMap`](https://atnt.github.io/Plot2Map/reference/predictBiasMap.md)
for generating wall-to-wall bias predictions,
[`invDasymetry`](https://atnt.github.io/Plot2Map/reference/invDasymetry.md)
for generating the input plot comparison data

## Examples

``` r
if (FALSE) { # \dontrun{
# Create synthetic rasters for demonstration
library(terra)
r <- rast(ncol = 100, nrow = 100, xmin = -10, xmax = 10, ymin = 35, ymax = 45,
          crs = "EPSG:4326")
map_agb <- setValues(r, runif(ncell(r), 50, 300))
map_sd <- setValues(r, runif(ncell(r), 10, 50))
height <- setValues(r, runif(ncell(r), 5, 30))
treecover <- setValues(r, runif(ncell(r), 20, 100))

# Create sample plot data (e.g., from invDasymetry output)
plot_data <- data.frame(
  x = runif(50, -5, 5),
  y = runif(50, 40, 45),
  plotAGB_10 = runif(50, 40, 250),
  mapAGB = runif(50, 60, 280)
)

# Create named list of covariates
covariates <- list(height = height, treecover = treecover)

# Extract bias and covariates at plot locations
bias_data <- extractBiasCovariates(
  plot_data = plot_data,
  map_agb = map_agb,
  map_sd = map_sd,
  covariates = covariates,
  plot_agb_col = "plotAGB_10",
  map_agb_col = "mapAGB"
)

# View results
head(bias_data)
cat("Mean bias:", mean(bias_data$bias, na.rm = TRUE), "Mg/ha\n")
cat("Bias range:", range(bias_data$bias, na.rm = TRUE), "Mg/ha\n")
} # }
```
