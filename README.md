
# Plot2Map <img src="man/figures/logo.png" align="right" height="138"/>

Plot2Map is an R package for comparing forest plot data to biomass maps, with a focus on Above Ground Biomass (AGB) estimation and validation.

## Installation

You can install Plot2Map from GitHub:

```R
# install.packages("devtools")
devtools::install_github("aTnT/Plot2Map")
```

## What Plot2Map can be used for

- Process various types of plot data (point data, polygons, tree-level measurements)
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

# Load and preprocess plot data
plots <- read.csv("SamplePlots.csv")
plots <- Deforested(plots, flDir, mapYear)
plots <- BiomePair(plots)

# Apply temporal adjustment
plots <- TempApply(plots, "Tropical", 2020)

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

1. Getting Started with Plot2Map

2. Plot Data Preparation

2.1 Plot Data Formats
2.2 Estimating Measurement and Sampling Errors
2.3 Temporal Adjustment of Plot Data

3. AGB and Treecover Data Preparation
3.1 Validating Global AGB Maps
3.2 Using Custom Forest Masks

4. Results Data Aggregation and Visualization
Aggregating Results at Different Spatial Scales
Visualizing Validation Results



## References

Araza et al., A comprehensive framework for assessing the accuracy and uncertainty of global above-ground biomass maps,
Remote Sensing of Environment, Volume 272, 2022, 112917, ISSN 0034-4257, https://doi.org/10.1016/j.rse.2022.112917.





