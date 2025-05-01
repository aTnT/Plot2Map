
# Plot2Map <img src="man/figures/logo.png" align="right" height="75"/>

Plot2Map is an R package for comparing forest plot data to biomass maps, with a focus on Above Ground Biomass (AGB) estimation and validation.

## Installation

You can install Plot2Map from GitHub:

```R
# install.packages("devtools")
devtools::install_github("aTnT/Plot2Map")
```

## What Plot2Map can be used for

- Pre-processing various types of plot data (point data, polygons, tree-level measurements, lidar)
- Temporal adjustment of plot data to match map epoch
- Estimation of measurement and sampling errors
- Validation of global AGB maps
- Support for custom forest masks
- Aggregation of results at different spatial scales


## Usage

### Plot data pre-processing

Here's a basic workflow using Plot2Map. Let's start by creating a sample dataset
of 10 plots.

```R
library(Plot2Map)

set.seed(42)
sampled_plots <- plots[sample(nrow(plots), 10), ]
print(sampled_plots)

#      PLOT_ID     POINT_X   POINT_Y  AGB_T_HA AVG_YEAR SIZE_HA
# 2369     EU2   1.3059145  42.59214  87.87177     2001   0.196
# 5273     EU2  -0.1585048  42.63245 150.97321     2004   0.196
# 1252     EU2  -1.0662342  39.61600  58.63558     2006   0.196
# 356      EU2  -5.1210978  40.02713  37.31206     2004   0.196
# 7700     EU1  15.2187262  59.81792  67.79700     2008   0.015
# 3954     EU2  -7.1823899  37.34714  94.59592     2007   0.196
# 5403     EU2  -7.2162433  37.34765  45.56841     2007   0.196
# 932      EU2  -2.7070556  42.68534  55.14102     2003   0.196
# 5637    AUS1 145.8683679 -20.69546  53.69000     2004   0.160
# 4002     EU2  -3.9889352  40.33963  46.49786     2000   0.196
```

We now preprocess the sampled plot data. The first step is to remove plots that
have been deforested based on the Global Forest Change (GFC) dataset (Hansen et al., 2013).
We use `Deforested()` that processes each plot, downloads the necessary GFC forest loss tiles,
and determines if the plot has been deforested beyond a set deforestation threshold or 
if the deforestation occurred before or during the specified map year. In the example
below we use the default 5% deforestation threshold, and assume a map year of 2023:

```R
sampled_plots <- Deforested(sampled_plots,  map_year = 2023)
print(sampled_plots)

# $non_deforested_plots
# Simple feature collection with 9 features and 4 fields
# Geometry type: POINT
# Dimension:     XY
# Bounding box:  xmin: -7.216243 ymin: -20.69546 xmax: 145.8684 ymax: 59.81792
# Geodetic CRS:  WGS 84
#   PLOT_ID  AGB_T_HA AVG_YEAR SIZE_HA                    geometry
# 1     EU2  87.87177     2001   0.196   POINT (1.305915 42.59214)
# 2     EU2 150.97321     2004   0.196 POINT (-0.1585048 42.63245)
# 3     EU2  58.63558     2006   0.196    POINT (-1.066234 39.616)
# 4     EU2  37.31206     2004   0.196  POINT (-5.121098 40.02713)
# 5     EU1  67.79700     2008   0.015   POINT (15.21873 59.81792)
# 6     EU2  45.56841     2007   0.196  POINT (-7.216243 37.34765)
# 7     EU2  55.14102     2003   0.196  POINT (-2.707056 42.68534)
# 8    AUS1  53.69000     2004   0.160  POINT (145.8684 -20.69546)
# 9     EU2  46.49786     2000   0.196  POINT (-3.988935 40.33963)
# 
# $all_plots
# Simple feature collection with 10 features and 6 fields
# Geometry type: POINT
# Dimension:     XY
# Bounding box:  xmin: -7.216243 ymin: -20.69546 xmax: 145.8684 ymax: 59.81792
# Geodetic CRS:  WGS 84
#      PLOT_ID  AGB_T_HA AVG_YEAR SIZE_HA                    geometry defo defo_start_year
# 2369     EU2  87.87177     2001   0.196   POINT (1.305915 42.59214)    0              NA
# 5273     EU2 150.97321     2004   0.196 POINT (-0.1585048 42.63245)    0              NA
# 1252     EU2  58.63558     2006   0.196    POINT (-1.066234 39.616)    0              NA
# 356      EU2  37.31206     2004   0.196  POINT (-5.121098 40.02713)    0              NA
# 7700     EU1  67.79700     2008   0.015   POINT (15.21873 59.81792)    0              NA
# 3954     EU2  94.59592     2007   0.196   POINT (-7.18239 37.34714)    1            2021
# 5403     EU2  45.56841     2007   0.196  POINT (-7.216243 37.34765)    0              NA
# 932      EU2  55.14102     2003   0.196  POINT (-2.707056 42.68534)    0              NA
# 5637    AUS1  53.69000     2004   0.160  POINT (145.8684 -20.69546)    0              NA
# 4002     EU2  46.49786     2000   0.196  POINT (-3.988935 40.33963)    0              NA
```

We can see that the function returns a list containing two elements:

* `$non_deforested_plots`: A `sf` object with non-deforested plots.

* `$all_plots`: The original input `sf` object with added deforestated proportion (0-1)
w.r.t. to plot area and deforestation start year.

In the next step we assign ecological zones and continents to plot locations. We use
`BiomePair()` for this. This function overlays plot locations with pre-processed data
to assign corresponding FAO ecological zones (biomes), Global Ecological Zones (GEZ)
and continents (zones) to each plot:

```R
sampled_plots <- BiomePair(sampled_plots$non_deforested_plots)
print(sampled_plots)

#   PLOT_ID  AGB_T_HA AVG_YEAR SIZE_HA     POINT_X   POINT_Y      ZONE                 FAO.ecozone         GEZ
# 1    AUS1  53.69000     2004   0.160 145.8683679 -20.69546 Australia          Tropical shrubland    Tropical
# 2     EU1  67.79700     2008   0.015  15.2187262  59.81792    Europe    Boreal coniferous forest      Boreal
# 3     EU2  87.87177     2001   0.196   1.3059145  42.59214    Europe   Temperate mountain system   Temperate
# 4     EU2  45.56841     2007   0.196  -7.2162433  37.34765    Europe      Subtropical dry forest Subtropical
# 5     EU2  58.63558     2006   0.196  -1.0662342  39.61600    Europe      Subtropical dry forest Subtropical
# 6     EU2  37.31206     2004   0.196  -5.1210978  40.02713    Europe      Subtropical dry forest Subtropical
# 7     EU2  46.49786     2000   0.196  -3.9889352  40.33963    Europe      Subtropical dry forest Subtropical
# 8     EU2 150.97321     2004   0.196  -0.1585048  42.63245    Europe   Temperate mountain system   Temperate
# 9     EU2  55.14102     2003   0.196  -2.7070556  42.68534    Europe Subtropical mountain system Subtropical
```

In the step below we use `TempApplyVar()` to adjust plot biomass values to align with
the map year by adding or subtracting annual growth increment to older or newer
plots. The function uses growth data from a model-based estimate of growth-age relationships.
The function also computes the temporal variance (standard deviation) of biomass changes based on growth-rate standard deviations.
In the example below we apply temporal adjustment and calculate variance to all Global Ecological Zones (GEZ)
within the plot data:

```R
sampled_plots <- TempApplyVar(sampled_plots, 2023)
print(sampled_plots)

#   PLOT_ID  AGB_T_HA AVG_YEAR SIZE_HA     POINT_X   POINT_Y      ZONE                 FAO.ecozone         GEZ AGB_T_HA_ORIG sdGrowth
# 1    AUS1 129.69000     2004   0.160 145.8683679 -20.69546 Australia          Tropical shrubland    Tropical      53.69000     76.0
# 2     EU1  84.29700     2008   0.015  15.2187262  59.81792    Europe    Boreal coniferous forest      Boreal      67.79700     16.5
# 3     EU2  87.87177     2001   0.196   1.3059145  42.59214    Europe   Temperate mountain system   Temperate      87.87177      0.0
# 4     EU2 109.56841     2007   0.196  -7.2162433  37.34765    Europe      Subtropical dry forest Subtropical      45.56841     64.0
# 5     EU2 126.63558     2006   0.196  -1.0662342  39.61600    Europe      Subtropical dry forest Subtropical      58.63558     68.0
# 6     EU2 113.31206     2004   0.196  -5.1210978  40.02713    Europe      Subtropical dry forest Subtropical      37.31206     76.0
# 7     EU2 138.49786     2000   0.196  -3.9889352  40.33963    Europe      Subtropical dry forest Subtropical      46.49786     92.0
# 8     EU2 150.97321     2004   0.196  -0.1585048  42.63245    Europe   Temperate mountain system   Temperate     150.97321      0.0
# 9     EU2 105.14102     2003   0.196  -2.7070556  42.68534    Europe Subtropical mountain system Subtropical      55.14102     50.0
```
Note how the `$AGB_T_HA`, `$AGB_T_HA_ORIG` and `$sdGrowth` columns are added to the plot dataset. The first corresponding 
to the adjusted biomass values for the specified `map_year`, the second keeping the original biomass values before adjustment and the 
last adding the temporal standard deviation of the biomass adjustment.



### AGB map validation

Now let's suppose we want to validate an AGB map on the `sampled_plots` data above.
We give an example of doing this with the `invDasymetry()` function where the AGB
map is the [ESA-CCI AGB dataset](https://dx.doi.org/10.5285/bf535053562141c6bb7ad831f5998d77).

This function performs inverse dasymetric mapping on plot data. It selects plots
based on given criteria, optionally aggregates them, and calculates forest
fraction and AGB for each plot or cell:


```R
# Validate AGB map
AGBdata <- invDasymetry(plot_data = sampled_plots,
                       clmn = "ZONE",
                       value = "Europe",
                       dataset = "esacci",
                       is_poly = FALSE,
                       threshold = 10)

# 8 plots being processed...
#   |                                                                                                       |   0%ℹ Loading Plot2Map
#   |=============                                                                                          |  12%1 tiles to download/check.
# 0 file(s) succeeded, 1 file(s) skipped, 0 file(s) failed.
# Processing tile: Hansen_GFC-2023-v1.11_treecover2000_60N_010E.tif
# Extracting values for ROI...
# Extraction complete
# Processing file: N60E010_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2021-fv5.0.tif
# File not found locally. Attempting to download...
# Downloading 1 ESA CCI Biomass v5.01 file(s) for year 2021...
#   |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=12s  
# Download successful: N60E010_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2021-fv5.0.tif
# Loading raster
# Extracting values for ROI...
# Extraction complete
# AGB values extracted (weighted mean = FALSE): 1 values
#
# (...)
#                       
# Processing complete. Results: 
# 
#          plotAGB_10 tfPlotAGB orgPlotAGB mapAGB SIZE_HA          x        y
# result.1   21.07425  84.29700   67.79700      0   0.015 15.2187262 59.81792
# result.2   87.87177  87.87177   87.87177     87   0.196  1.3059145 42.59214
# result.3  109.56841 109.56841   45.56841     47   0.196 -7.2162433 37.34765
# result.4  110.80614 126.63558   58.63558     11   0.196 -1.0662342 39.61600
# result.5    0.00000 113.31206   37.31206      7   0.196 -5.1210978 40.02713
# result.6  138.49786 138.49786   46.49786      1   0.196 -3.9889352 40.33963
# result.7    0.00000 150.97321  150.97321     72   0.196 -0.1585048 42.63245
# result.8  105.14102 105.14102   55.14102     89   0.196 -2.7070556 42.68534                   
```

Now that we have the validation data from `invDasymetry()`, we can analyze and visualize the results. The function returns a data frame with several columns, including:

- `plotAGB_[threshold]`: AGB values adjusted for forest cover threshold (e.g., plotAGB_10)
- `tfPlotAGB`: Tree-filtered plot AGB (only when not aggregated)
- `orgPlotAGB`: Original plot AGB values
- `mapAGB`: The AGB values extracted from the map
- `SIZE_HA`: Plot size in hectares
- `x` and `y`: Plot coordinates

We can now visualize the relationship between the reference plot data and map values using the plotting functions:

```R
# Create output directory for results
out_dir <- "results"
dir.create(out_dir, showWarnings = FALSE)

# Visualize results with binned plot
Binned(AGBdata$plotAGB_10, AGBdata$mapAGB, 
       "Europe - Binned Comparison", 
       "binned_Europe.png", 
       outDir = out_dir)

# Create a scatter plot
Scatter(AGBdata$plotAGB_10, AGBdata$mapAGB, 
        "Europe - Scatter Plot", 
        "scatter_Europe.png", 
        outDir = out_dir)

# Calculate accuracy metrics
# Use fewer intervals since we have a small sample
accuracy_results <- Accuracy(df = AGBdata, intervals = 6, dir = out_dir, str = "Europe")
print(accuracy_results)
#    AGB bin (Mg/ha)  n AGBref (Mg/ha) AGBmap (Mg/ha) RMSD varPlot
# 1            0-100  4             27             42   38       1
# 2          100-150  4            116             37   91       1
# 19           total 24             72             39   70       1
```

The output of the `Accuracy()` function provides a comprehensive assessment of map accuracy, including:

- Binned statistics by AGB ranges
- Mean Square Deviation (MSD)
- Plot variance
- Map variance
- Overall statistics like RMSE, bias, and R²

### Handling Uncertainty

Plot2Map also provides tools for calculating and incorporating uncertainty in your analyses. The `calculateTotalUncertainty()` function combines measurement, sampling, and growth uncertainties:

```R
# Calculate total uncertainty
uncertainty_results <- calculateTotalUncertainty(sampled_plots, 
                                               map_year = 2023,
                                               map_resolution = 100)
# Calculating tree measurement uncertainty using RF model
# Calculating sampling uncertainty using Rejou-Mechain approach
# Using existing growth uncertainty (sdGrowth) values
# Total uncertainty calculated for plot data of type: point                                               

# Print the uncertainty components
print(uncertainty_results$uncertainty_components)
# measurement    sampling      growth 
#   0.2323145   0.2530749   0.5146106 
```

### Working with Different Plot Types

Plot2Map can handle various types of plot data:

1. **Point data with AGB estimates**:
   - Pre-formatted (see documentation)
   - Unformatted (can be processed with `RawPlots()`)

2. **Polygon data with corner coordinates**:
   - Can be processed with `Polygonize()`

3. **Tree-level measurement data**:
   - Processed with `MeasurementErr()` which estimates AGB and uncertainty

4. **Nested plot data (with sub-plots)**:
   - Processed with `Nested()` followed by `MeasurementErr()`

5. **Lidar-based reference maps**:
   - Processed with `RefLidar()`
   
   
### Aggregated Analysis and Spatial Correlation

Plot2Map allows for spatial aggregation of plots into larger cells for analysis at different scales:

```R
# Aggregated analysis at 0.1 degree resolution with minimum 3 plots per cell
AGBdata_agg <- invDasymetry(plot_data = sampled_plots,
                           clmn = "ZONE", 
                           value = "Europe",
                           aggr = 0.1,         # 0.1 degree aggregation
                           minPlots = 3,       # Minimum 3 plots per cell
                           dataset = "esacci",
                           is_poly = FALSE)

# Visualize aggregated results
Binned(AGBdata_agg$plotAGB_10, AGBdata_agg$mapAGB, 
      "Europe - Aggregated", 
      "binned_agg_Europe.png",
      outDir = out_dir)
```

For spatial correlation analysis, you can use:

```R
# Calculate spatial correlation parameters
spatcor_results <- SpatCor(AGBdata, distance_km = 100)
print(spatcor_results)
```

### Complete Workflow Example

Here's a complete workflow example combining all the key steps:

```R
library(Plot2Map)

# Load and preprocess plot data
data(plots)

# Select plots from a specific region
europe_plots <- plots[plots$ZONE == "Europe", ]

# Remove deforested plots
non_deforested <- Deforested(europe_plots, map_year = 2020)$non_deforested_plots

# Add ecological zone information
eco_plots <- BiomePair(non_deforested)

# Calculate total uncertainty including all components
plots_with_uncertainty <- calculateTotalUncertainty(
  eco_plots, 
  map_year = 2020, 
  map_resolution = 100
)

# Validate against ESA-CCI biomass map
validation_results <- invDasymetry(
  plot_data = plots_with_uncertainty$data,
  clmn = "GEZ",
  value = "Temperate", 
  dataset = "esacci",
  threshold = 10
)

# Create output directory
results_dir <- "validation_results"
dir.create(results_dir, showWarnings = FALSE)

# Visualize results
Binned(validation_results$plotAGB_10, validation_results$mapAGB,
      "Temperate Zone Validation", 
      "temperate_validation.png",
      outDir = results_dir)

# Calculate accuracy metrics
accuracy_stats <- Accuracy(df = validation_results, intervals = 6, 
                          dir = results_dir, 
                          str = "temperate_2020")

# Print summary statistics
print(accuracy_stats)
```


## Vignettes

* Plot data preparation



## References

Araza et al., A comprehensive framework for assessing the accuracy and uncertainty of global above-ground biomass maps,
Remote Sensing of Environment, Volume 272, 2022, 112917, ISSN 0034-4257, https://doi.org/10.1016/j.rse.2022.112917.





