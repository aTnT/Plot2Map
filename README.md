
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

Here's a basic workflow using Plot2Map:

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
sampled_plots <- Deforested(sampled_plots,  map_year = 2023)
sampled_plots <- BiomePair(sampled_plots$non_deforested_plots)

# Apply temporal adjustment
sampled_plots <- TempApply(sampled_plots, "Subtropical", 2023)

# Estimate errors, using a pre-trained RF model for plot-level data
load('rf1.RData') #pre-trained RF model from 10000+ plots across biomes
plotsPred <- plots2[,c('AGB_T_HA','SIZE_HA', 'GEZ')]
names(plotsPred) <- c('agb', 'size', 'gez')
plotsPred$size <- as.numeric(plotsPred$size) * 10000 #convert size to m2
plotsPred$gez = factor(plotsPred$gez,levels = c("Boreal","Subtropical","Temperate","Tropical"))
plots2$sdTree <- predict(rf1, plotsPred)[[1]]

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





