# Comprehensive uncertainty calculation for all plot data types

This function provides a unified framework for calculating uncertainty
across different plot data types. It automatically detects the plot data
type, preserves existing uncertainty components if present, and
calculates missing components as needed.

## Usage

``` r
calculateTotalUncertainty(
  plot_data,
  map_year,
  map_resolution = 100,
  biome_info = TRUE,
  se_data = NULL
)
```

## Arguments

- plot_data:

  A data frame containing plot data. Required columns:

  - `PLOT_ID` - Plot identifier

  - `AGB_T_HA` - Above-ground biomass in tons per hectare

  - `SIZE_HA` - Plot size in hectares

  - `GEZ` - Global ecological zone (e.g., "Tropical", "Temperate",
    "Boreal", "Subtropical")

  Additional columns required when `biome_info=TRUE`:

  - `ZONE` - Continental zone (e.g., "Europe", "Asia", "Africa")

  - `AVG_YEAR` - Year of plot measurement for temporal adjustment

  If `sdTree`, `sdSE`, or `sdGrowth` columns exist, they will be
  preserved.

- map_year:

  Numeric value indicating the map year for temporal adjustment. This is
  used to calculate growth uncertainty based on the time difference
  between plot measurement and map production.

- map_resolution:

  Numeric value indicating the map resolution in meters (default: 100).
  This affects sampling error calculations based on the difference
  between plot size and pixel size.

- biome_info:

  Logical indicating whether to use biome information (GEZ column) for
  growth uncertainty calculation (default: TRUE). When TRUE, the
  function uses TempVar which requires 'ZONE', 'GEZ', and 'AVG_YEAR'
  columns to be present in the plot_data. If FALSE, the function
  requires that `sdGrowth` column be provided in the input data.

- se_data:

  Optional data frame with sampling error data that can be used instead
  of loading se.csv. Must contain columns:

  - `SIZE_HA` - Plot size in hectares

  - `RS_HA` - Pixel resolution in hectares

  - `ratio` - Ratio of plot size to pixel size

  - `cv` - Coefficient of variation (percent)

## Value

A list containing:

- data:

  The input plot data with additional uncertainty columns:

  - `sdTree` - Standard deviation from measurement error

  - `sdSE` - Standard deviation from sampling error

  - `sdGrowth` - Standard deviation from growth/temporal uncertainty

  - `varPlot` - Total variance (sum of squared standard deviations)

  - `sdTotal` - Total standard deviation (square root of varPlot)

- plot_type:

  The detected plot data type: "tree_level", "point", "polygon",
  "nested", or "lidar"

- uncertainty_components:

  The relative contribution of each uncertainty component as a
  proportion of the total variance

## Details

The function handles three primary sources of uncertainty:

- **Measurement uncertainty (sdTree)**: Derived from allometric
  equations, wood density estimates, and measurement error. For
  tree-level data, this is calculated by the BIOMASS package with Monte
  Carlo methods. For plot-level data, it's estimated using a random
  forest model trained on a large dataset.

- **Sampling uncertainty (sdSE)**: Accounts for the sampling error
  introduced by differing plot sizes vs. pixel sizes, following the
  approach from Réjou-Méchain et al. (2014).

- **Growth uncertainty (sdGrowth)**: Adjusts for temporal differences
  between plot measurement year and map production year, using growth
  rates specific to each ecological zone.

These components are combined using error propagation principles:
\$\$varPlot = sdTree^2 + sdSE^2 + sdGrowth^2\$\$ \$\$sdTotal =
\sqrt{varPlot}\$\$

## Calculation Process

### Step 1: Measurement Error (sdTree)

- **For tree-level data**: Uses BIOMASS package Monte Carlo simulations
  that account for wood density uncertainty, diameter measurement
  errors, and allometric model errors.

- **For plot-level data**: Uses a random forest model trained on
  thousands of plots across biomes to predict measurement uncertainty
  based on plot AGB, plot size, and ecological zone.

- If a pre-existing sdTree column exists, its values are preserved.

### Step 2: Sampling Error (sdSE)

- Uses a random forest model trained on geo-simulation data that relates
  plot-to-pixel size ratios to coefficient of variation.

- The model considers the relationship between plot size (SIZE_HA),
  remote sensing pixel size (RS_HA), and their ratio.

- The resulting coefficient of variation is converted to standard
  deviation by multiplying by mean AGB.

- Custom sampling error data can be provided through the `se_data`
  parameter.

### Step 3: Growth Uncertainty (sdGrowth)

- If `biome_info = TRUE`, uses biome-specific growth rates and
  uncertainty from the TempVar function.

- The function considers the time difference between plot measurement
  year (AVG_YEAR) and map year.

- Each ecological zone (GEZ) is processed separately to apply
  biome-specific growth rates.

- If biome information isn't available or `biome_info = FALSE`, the
  function requires that `sdGrowth` column be provided in the input
  data.

### Step 4: Total Uncertainty

- Combines all uncertainty components by summing their variances:
  `varPlot = sdTree^2 + sdSE^2 + sdGrowth^2`

- Calculates total standard deviation as `sdTotal = sqrt(varPlot)`

- Reports the relative contribution of each component to the total
  variance.

## Examples

``` r
# Example 1: Using existing plot data
# First load the sample plots dataset included with the package
data(plots)

# Create a subset of plots for demonstration
set.seed(42)
sample_plots <- plots[sample(nrow(plots), 5), ]

# Add biome information (required for uncertainty calculation)
sample_plots <- BiomePair(sample_plots)

# Apply TempApplyVar to adjust biomass to map year and calculate temporal variance
sample_plots <- TempApplyVar(sample_plots, map_year = 2020)

# Calculate total uncertainty components for map year 2020
result <- calculateTotalUncertainty(sample_plots, map_year = 2020, map_resolution = 100)
#> Calculating tree measurement uncertainty using RF model
#> Calculating sampling uncertainty using Rejou-Mechain approach
#> Loading sampling error data from package file
#> Using existing growth uncertainty (sdGrowth) values
#> Total uncertainty calculated for plot data of type: point

# View the results
head(result$data)
#>   PLOT_ID AGB_T_HA AVG_YEAR SIZE_HA    POINT_X  POINT_Y   ZONE
#> 1     EU1  80.9970     2008   0.015 15.2187262 59.81792 Europe
#> 2     EU2 146.7718     2001   0.196  1.3059145 42.59214 Europe
#> 3     EU2 114.6356     2006   0.196 -1.0662342 39.61600 Europe
#> 4     EU2 101.3121     2004   0.196 -5.1210978 40.02713 Europe
#> 5     EU2 150.9732     2004   0.196 -0.1585048 42.63245 Europe
#>                 FAO.ecozone         GEZ AGB_T_HA_ORIG sdGrowth   sdTree RS_HA
#> 1  Boreal coniferous forest      Boreal      67.79700     13.2 29.81063     1
#> 2 Temperate mountain system   Temperate      87.87177     58.9 24.96373     1
#> 3    Subtropical dry forest Subtropical      58.63558     56.0 30.15383     1
#> 4    Subtropical dry forest Subtropical      37.31206     64.0 28.39875     1
#> 5 Temperate mountain system   Temperate     150.97321      0.0 24.53302     1
#>   ratio     sdSE  varPlot  sdTotal
#> 1 0.015 52.88286 3859.511 62.12496
#> 2 0.196 22.81788 4613.054 67.91946
#> 3 0.196 22.81788 4565.910 67.57151
#> 4 0.196 22.81788 5423.145 73.64200
#> 5 0.196 22.81788 1122.525 33.50411

# See the relative contribution of each uncertainty component
result$uncertainty_components
#> measurement    sampling      growth 
#>   0.2498749   0.3022655   0.4478596 

# Example 2: Working with data that already has measurement uncertainty
# First process tree-level data to get measurement uncertainty
# Load sample tree data
tree_data <- utils::read.csv(sample_file("SampleTree.csv"))
tree_locations <- utils::read.csv(sample_file("SampleTreeXY.csv"))

# Calculate AGB and measurement uncertainty
tree_results <- sd_tree(tree_data, tree_locations, region = "World")
#> Using useCache=TRUE is recommended to reduce online search time for the next query
#>   |                                                                              |                                                                      |   0%
#> Warning: There seem to be a problem reaching the TNRS API!
#> Failed to perform HTTP request.
#> Taxonomic correction attempt 1 failed: Invalid taxonomic correction results (empty or length mismatch). Retrying in 1 seconds...
#> Using useCache=TRUE is recommended to reduce online search time for the next query
#>   |                                                                              |                                                                      |   0%
#> Warning: There seem to be a problem reaching the TNRS API!
#> Failed to perform HTTP request.
#> Taxonomic correction attempt 2 failed: Invalid taxonomic correction results (empty or length mismatch). Retrying in 1 seconds...
#> Using useCache=TRUE is recommended to reduce online search time for the next query
#>   |                                                                              |                                                                      |   0%
#> Warning: There seem to be a problem reaching the TNRS API!
#> Failed to perform HTTP request.
#> Taxonomic correction failed after 3 attempts: Invalid taxonomic correction results (empty or length mismatch). Using original taxonomic names.
#> Warning: Taxonomic correction failed after all retry attempts. Using original taxonomic names.
#> The reference dataset contains 16467 wood density values
#> Your taxonomic table contains 340 taxa
#> No tree height data found in original plot data. Calculating height using BIOMASS height-diameter model.

# Add required biome information
tree_results <- BiomePair(tree_results)

# Calculate total uncertainty with tree data
tree_uncertainty <- calculateTotalUncertainty(tree_results, map_year = 2020)
#> Using existing sdTree values for tree_level data
#> Calculating sampling uncertainty using Rejou-Mechain approach
#> Loading sampling error data from package file
#> Calculating growth uncertainty by biome
#> Total uncertainty calculated for plot data of type: tree_level

# Example 3: Using custom sampling error data
# Create custom sampling error data (coefficient of variation by plot/pixel size ratio)
custom_se_data <- data.frame(
  SIZE_HA = c(0.1, 0.25, 0.5, 1.0),  # Plot sizes in hectares
  RS_HA = c(1, 1, 1, 1),            # Remote sensing pixel size in hectares
  ratio = c(0.1, 0.25, 0.5, 1.0),   # Ratio of plot size to pixel size
  cv = c(20, 15, 10, 5)             # Coefficient of variation (%)
)

# Calculate uncertainty using custom sampling error model
custom_result <- calculateTotalUncertainty(
  sample_plots,
  map_year = 2020,
  map_resolution = 100,
  se_data = custom_se_data
)
#> Calculating tree measurement uncertainty using RF model
#> Calculating sampling uncertainty using Rejou-Mechain approach
#> Using provided sampling error data
#> Using existing growth uncertainty (sdGrowth) values
#> Total uncertainty calculated for plot data of type: point

# Example 4: Using manual growth uncertainty values
# First, prepare data with manual sdGrowth values
manual_growth_plots <- sample_plots
# Add manual growth uncertainty of 7% of AGB
manual_growth_plots$sdGrowth <- manual_growth_plots$AGB_T_HA * 0.07

# Now calculate uncertainty with biome_info = FALSE
simple_result <- calculateTotalUncertainty(
  manual_growth_plots,
  map_year = 2020,
  map_resolution = 100,
  biome_info = FALSE
)
#> Calculating tree measurement uncertainty using RF model
#> Calculating sampling uncertainty using Rejou-Mechain approach
#> Loading sampling error data from package file
#> Using existing growth uncertainty (sdGrowth) values
#> Total uncertainty calculated for plot data of type: point
```
