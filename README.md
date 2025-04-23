
# Plot2Map <img src="man/figures/logo.png" align="right" height="75"/>

Plot2Map is an R package for comparing forest plot data to biomass maps, with a focus on Above Ground Biomass (AGB) estimation and validation.

## Installation

You can install Plot2Map from GitHub:

```R
# install.packages("devtools")
devtools::install_github("aTnT/Plot2Map")
```

## What Plot2Map can be used for

- Process various types of plot data (point data, polygons, tree-level measurements, lidar)
- Temporal adjustment of plot data to match map epoch
- Estimation of measurement and sampling errors
- Validation of global AGB maps
- Support for custom forest masks
- Aggregation of results at different spatial scales
- Visualization tools for results (scatter plots, binned comparisons)


## Usage

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

In the step below we use `TempApply()` to adjust plot biomass values to align with
the map year by adding or subtracting annual growth increment to older or newer
plots. The function uses growth data from a model-based estimate of growth-age relationships.
In the example below we apply temporal adjustment to all Global Ecological Zones (GEZ)
within the plot data:

```R
sampled_plots <- TempApply(sampled_plots, 2023)
print(sampled_plots)

#   PLOT_ID  AGB_T_HA AVG_YEAR SIZE_HA     POINT_X   POINT_Y      ZONE                 FAO.ecozone         GEZ AGB_T_HA_ORIG
# 1    AUS1 129.69000     2004   0.160 145.8683679 -20.69546 Australia          Tropical shrubland    Tropical      53.69000
# 2     EU1  84.29700     2008   0.015  15.2187262  59.81792    Europe    Boreal coniferous forest      Boreal      67.79700
# 3     EU2  87.87177     2001   0.196   1.3059145  42.59214    Europe   Temperate mountain system   Temperate      87.87177
# 4     EU2 109.56841     2007   0.196  -7.2162433  37.34765    Europe      Subtropical dry forest Subtropical      45.56841
# 5     EU2 126.63558     2006   0.196  -1.0662342  39.61600    Europe      Subtropical dry forest Subtropical      58.63558
# 6     EU2 113.31206     2004   0.196  -5.1210978  40.02713    Europe      Subtropical dry forest Subtropical      37.31206
# 7     EU2 138.49786     2000   0.196  -3.9889352  40.33963    Europe      Subtropical dry forest Subtropical      46.49786
# 8     EU2 150.97321     2004   0.196  -0.1585048  42.63245    Europe   Temperate mountain system   Temperate     150.97321
# 9     EU2 105.14102     2003   0.196  -2.7070556  42.68534    Europe Subtropical mountain system Subtropical      55.14102

```


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
                       dataset = "esacci")
```

xxx

```R
# Visualize results
Binned(AGBdata$plotAGB_10, AGBdata$mapAGB, "Europe", "binned_plot.png")
Scatter(AGBdata$plotAGB_10, AGBdata$mapAGB, "Europe", "scatter_plot.png")

# Calculate accuracy metrics
Accuracy(AGBdata, 8, outDir, "ValidationRun")
```



```R
library(Plot2Map)

# Create a sample dataset of 10 plots:
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

# Preprocess plot data:
sampled_plots <- Deforested(sampled_plots,  map_year = 2006)
sampled_plots <- BiomePair(sampled_plots$non_deforested_plots)

# Apply temporal adjustment
sampled_plots <- TempApply(sampled_plots, map_year = 2006)

# Estimate errors, using a pre-trained RF model for plot-level data
load('data/rf1.RData') #pre-trained RF model from 10000+ plots across biomes
plotsPred <- sampled_plots[,c('AGB_T_HA','SIZE_HA', 'GEZ')]
names(plotsPred) <- c('agb', 'size', 'gez')
plotsPred$size <- as.numeric(plotsPred$size) * 10000 #convert size to m2
plotsPred$gez = factor(plotsPred$gez,levels = c("Boreal","Subtropical","Temperate","Tropical"))
sampled_plots$sdTree <- predict(rf1, plotsPred)[[1]]

# Validate AGB map
AGBdata <- invDasymetry("ZONE", "Europe", wghts = TRUE, is_poly = FALSE, own = FALSE)

# Visualize results
Binned(AGBdata$plotAGB_10, AGBdata$mapAGB, "Europe", "binned_plot.png")
Scatter(AGBdata$plotAGB_10, AGBdata$mapAGB, "Europe", "scatter_plot.png")

# Calculate accuracy metrics
Accuracy(AGBdata, 8, outDir, "ValidationRun")
```


## Vignettes

* Getting started with Plot2Map
* Plot data preparation
* Uncertainty quantification
* Map bias assessment and correction



## References

Araza et al., A comprehensive framework for assessing the accuracy and uncertainty of global above-ground biomass maps,
Remote Sensing of Environment, Volume 272, 2022, 112917, ISSN 0034-4257, https://doi.org/10.1016/j.rse.2022.112917.





