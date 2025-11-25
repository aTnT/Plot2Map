# Global Reference Dataset for Above-Ground Biomass (AGBref)

This dataset is part of the AGBref initiative, a global reference
dataset for above-ground biomass (AGB) validation derived from national
forest inventories (NFIs), permanent research plots, and local airborne
LiDAR-based maps. It contains harmonized AGB estimates across multiple
spatial resolutions and epochs, together with associated uncertainties
and ecological metadata. It is primarily intended to support the
validation of satellite-derived global biomass maps and national carbon
accounting.

## Usage

``` r
AGBref
```

## Format

### `AGBref`

A data frame containing global reference AGB measurements:

- POINT_X:

  Longitude of the grid cell centroid.

- POINT_Y:

  Latitude of the grid cell centroid.

- TC_PLT_SD:

  Standard deviation of tree cover (%) at the plot locations within the
  grid cell, based on remote sensing, is used to assess variability in
  plot representativeness.

- TC_PLT_MEAN:

  Mean tree cover (%) at the plot locations within the grid cell.

- TC_GRID_SD:

  Standard deviation of tree cover (%) at the entire grid cell (not just
  at plot locations).

- TC_GRID_MEAN:

  Mean tree cover (%) at the grid cell level, derived from satellite
  data (e.g., Global Forest Change dataset).

- n:

  Number of plots used within the grid cell to compute biomass
  estimates.

- AGB_T_HA:

  Harmonized above-ground biomass (in tons per hectare, T/ha) at the
  grid cell level. Adjusted for forest area definition, temporal
  mismatch, and other preprocessing steps.

- SIZE_HA:

  Total plot area within the grid cell in hectares.

- OPEN:

  Indicator for openness of data.

- VER:

  Denotes versioning or verification status. To be updated when new
  versions are released.

- varTot:

  Total variance of AGB within the grid cell. Accounts for the
  spatial-scale mismatch considering the target grid cell and associated
  measurement-related (tree measurement, allometric model use)
  uncertainties.

- AVG_YEAR:

  Average year of plot measurement for plots within the grid cell.
  Useful for matching map epochs (e.g., 2005, 2010, etc.).

- BIO:

  Biome category of the grid cell, e.g., "Tropical rainforest",
  "Temperate broadleaf and mixed forests". Derived from ecological zone
  datasets.

- CODE:

  Source code of the dataset provider. Example: AUS1 for Australia-based
  NFI. Refer to supplementary table in the manuscript.

- INVENTORY:

  Type or scale of data source, e.g., regional, national, or local.

## Source

<https://doi.org/10.5281/zenodo.15495069>

## Note

This should be considered a version 0 and will immediately updated with
a more comprehensive version alongside peer review process of the data
descriptor paper.

## Examples

``` r
# Load the AGBref dataset
data("AGBref", package = "Plot2Map")

# Explore the dataset
head(AGBref)
#>          POINT_X POINT_Y TC_PLT_SD TC_PLT_MEAN TC_GRID_SD TC_GRID_MEAN  n
#> result.1  146.85  -43.55  6.350853    86.33333  10.733498     86.24031  3
#> result.2  146.85  -43.45  0.000000    90.00000   7.256462     87.37189  4
#> result.3  146.85  -43.35  3.326275    88.30769   8.816047     87.74057 13
#> result.4  146.95  -43.35  0.000000    90.00000  11.923481     85.43528 21
#> result.5  147.05  -43.35  3.502380    88.60000  14.653529     82.91692 10
#> result.6  147.25  -43.35 45.000000    67.50000  15.142176     82.58522  4
#>          AGB_T_HA    SIZE_HA OPEN VER    varTot AVG_YEAR
#> result.1 940.1520 0.04523893    0   1 20448.331     2011
#> result.2 432.0948 0.04523893    0   1 11875.633     2012
#> result.3 668.1510 0.04523893    0   1  3099.880     2013
#> result.4 216.0863 0.04523893    0   1  1528.960     2013
#> result.5 144.2624 0.04523893    0   1  4298.584     2013
#> result.6 333.9161 0.04523893    0   1  9841.693     2010
#>                                            BIO CODE INVENTORY  TIER
#> result.1 Temperate broadleaf and mixed forests AUS1  regional tier1
#> result.2 Temperate broadleaf and mixed forests AUS1  regional tier1
#> result.3 Temperate broadleaf and mixed forests AUS1  regional tier1
#> result.4 Temperate broadleaf and mixed forests AUS1  regional tier1
#> result.5 Temperate broadleaf and mixed forests AUS1  regional tier1
#> result.6 Temperate broadleaf and mixed forests AUS1  regional tier1
summary(AGBref)
#>     POINT_X           POINT_Y         TC_PLT_SD       TC_PLT_MEAN    
#>  Min.   :-166.45   Min.   :-43.55   Min.   : 0.000   Min.   :  0.00  
#>  1st Qu.: -46.65   1st Qu.: -7.15   1st Qu.: 3.361   1st Qu.: 15.60  
#>  Median :   8.95   Median : 33.85   Median :11.369   Median : 63.75  
#>  Mean   :  11.24   Mean   : 22.04   Mean   :15.827   Mean   : 53.40  
#>  3rd Qu.:  22.65   3rd Qu.: 44.75   3rd Qu.:26.795   3rd Qu.: 85.00  
#>  Max.   : 162.25   Max.   : 78.75   Max.   :70.711   Max.   :100.00  
#>                                     NA's   :14576                    
#>    TC_GRID_SD     TC_GRID_MEAN         n              AGB_T_HA       
#>  Min.   : 0.00   Min.   :10.00   Min.   :   1.00   Min.   :   0.000  
#>  1st Qu.:14.46   1st Qu.:50.96   1st Qu.:   1.00   1st Qu.:   6.547  
#>  Median :18.56   Median :68.17   Median :   2.00   Median :  28.044  
#>  Mean   :17.76   Mean   :64.44   Mean   :  14.73   Mean   :  61.110  
#>  3rd Qu.:21.98   3rd Qu.:78.84   3rd Qu.:   6.00   3rd Qu.:  72.745  
#>  Max.   :35.49   Max.   :99.99   Max.   :7847.00   Max.   :2631.873  
#>  NA's   :851     NA's   :837                       NA's   :606       
#>     SIZE_HA               OPEN             VER            varTot         
#>  Min.   :  0.01009   Min.   :0.0000   Min.   :1.000   Min.   :6.100e-02  
#>  1st Qu.:  0.06400   1st Qu.:0.0000   1st Qu.:1.000   1st Qu.:3.436e+02  
#>  Median :  0.12566   Median :0.0000   Median :3.000   Median :5.479e+02  
#>  Mean   :  2.79386   Mean   :0.1431   Mean   :3.493   Mean   :2.393e+03  
#>  3rd Qu.:  0.50000   3rd Qu.:0.0000   3rd Qu.:6.000   3rd Qu.:1.459e+03  
#>  Max.   :100.00000   Max.   :1.0000   Max.   :6.000   Max.   :1.364e+05  
#>                                                                          
#>     AVG_YEAR   
#>  Min.   :1900  
#>  1st Qu.:2006  
#>  Median :2013  
#>  Mean   :2012  
#>  3rd Qu.:2017  
#>  Max.   :2024  
#>                
#>                                                            BIO      
#>  Temperate broadleaf and mixed forests                       :8645  
#>  Tropical and subtropical moist broadleaf forest             :6602  
#>  Mediterranean forests, woodland and scrub                   :5726  
#>  Tropical and subtropical grasslands, savannas and shrublands:4612  
#>  Boreal forests                                              :3140  
#>  Tropical and subtropical dry broadleaf forest               :2401  
#>  (Other)                                                     :3912  
#>       CODE          INVENTORY        TIER      
#>  SAM_BRA: 7933   LIDAR   :  220        :16607  
#>  EU2    : 3966   local   : 5859   tier0:  897  
#>  ASI_JAP: 2956   national:15194   tier1:17363  
#>  EU_SWE : 2619   regional:13765   tier2:  169  
#>  EU1    : 2340                    tier3:    2  
#>  EU_ITAL: 2186                                 
#>  (Other):13038                                 

# Check available regions
table(AGBref$CODE)
#> 
#>  AFR_COF  AFR_FOS  AFR_GHA  AFR_KEN  AFR_LDR     AFR1    AFR10    AFR11 
#>      897       20        5       11       27       99        4      156 
#>    AFR12    AFR13    AFR14    AFR15     AFR2     AFR4     AFR5     AFR6 
#>        5        2        6      100       16       26        6       18 
#>     AFR7     AFR8     AFR9      ARC  ASI_FOS  ASI_ind  ASI_JAP  ASI_LDR 
#>        6       75      674      164        2      217     2956       25 
#>  ASI_nep ASI_neps  ASI_pak   ASI_PH     ASI1    ASI10    ASI11     ASI2 
#>      417      266      224      139       55      186      782       19 
#>     ASI3     ASI4     ASI9  AUS_FOS AUS_tern     AUS1     CAM1   EU_BEL 
#>       15       18        2        1       55     2078     1680      128 
#>   EU_CZH   EU_CZR   EU_FOS   EU_IRE  EU_ITAL   EU_POL   EU_SWE      EU1 
#>      481        4       52      662     2186        8     2619     2340 
#>      EU2      EU3      EU4   FOS_23 NAM_juni NAM_NEON NAM_tund     NAM1 
#>     3966      413      640        6      109      233       17      132 
#>     NAM2     NAM3     NAM4 SAM_BAJO  SAM_BRA  SAM_FOS  SAM_guy  SAM_KEL 
#>       27       67      685        5     7933       35      110      565 
#>   SAM_RF SAM_TAPA     SAM2     SAM3     SAM4     SAM5 
#>      110        8       28       10        2        3 

# Check data quality tiers
table(AGBref$TIER)
#> 
#>       tier0 tier1 tier2 tier3 
#> 16607   897 17363   169     2 

# Filter to Spain (EU2) - tier1 quality
spain_ref <- subset(AGBref, CODE == "EU2" & TIER == "tier1")
cat(sprintf("Spain: %d reference cells\\n", nrow(spain_ref)))
#> Spain: 3966 reference cells\n
cat(sprintf("Average plots per cell: %.1f\\n", mean(spain_ref$n)))
#> Average plots per cell: 14.3\n
cat(sprintf("AGB range: %.1f - %.1f Mg/ha\\n",
            min(spain_ref$AGB_T_HA, na.rm = TRUE),
            max(spain_ref$AGB_T_HA, na.rm = TRUE)))
#> AGB range: 0.0 - 169.9 Mg/ha\n

# AGBref is already preprocessed with:
# - Aggregation to 0.1 degree resolution
# - Total uncertainty (varTot column)
# - Multiple plots per cell (n column)
# - Quality tiers (TIER column)

if (FALSE) { # \dontrun{
# Example: Map validation workflow
# AGBref provides plot reference data, you need to add map values

# 1. Prepare data for validation
validation_data <- spain_ref
validation_data$x <- validation_data$POINT_X
validation_data$y <- validation_data$POINT_Y
validation_data$plotAGB_10 <- validation_data$AGB_T_HA
validation_data$varPlot <- validation_data$varTot

# 2. Extract map AGB values at reference locations
# (assuming you have loaded an AGB map as 'agb_map')
# validation_data$mapAGB <- terra::extract(agb_map,
#                                          validation_data[, c("x", "y")])[,2]

# 3. Visualize validation results
# Binned(validation_data$plotAGB_10, validation_data$mapAGB,
#        "Spain AGB Validation")

# 4. Calculate accuracy metrics
# Accuracy(validation_data, intervals = 6)

# 5. Use with bias modeling functions (see ?trainBiasModel)
# bias_data <- extractBiasCovariates(
#   plot_data = validation_data,
#   map_agb = agb_map,
#   map_sd = sd_map,
#   covariates = list(height = height_raster, treecover = tc_raster)
# )
# bias_model <- trainBiasModel(bias_data, predictors = c("map", "sd", "height"))

# 6. Use with advanced uncertainty functions (see ?fitResidualVariogram)
# vgm_fit <- fitResidualVariogram(
#   bias_data = bias_data,
#   map_sd_raster = sd_map,
#   cutoff = 200
# )
} # }

# Example: Compare different regions
brazil <- subset(AGBref, CODE == "SAM_BRA")
japan <- subset(AGBref, CODE == "ASI_JAP")
sweden <- subset(AGBref, CODE == "EU_SWE")

cat(sprintf("\\nRegional coverage:\\n"))
#> \nRegional coverage:\n
cat(sprintf("Brazil: %d cells, mean AGB = %.1f Mg/ha\\n",
            nrow(brazil), mean(brazil$AGB_T_HA, na.rm = TRUE)))
#> Brazil: 7933 cells, mean AGB = 34.2 Mg/ha\n
cat(sprintf("Japan: %d cells, mean AGB = %.1f Mg/ha\\n",
            nrow(japan), mean(japan$AGB_T_HA, na.rm = TRUE)))
#> Japan: 2956 cells, mean AGB = 21.6 Mg/ha\n
cat(sprintf("Sweden: %d cells, mean AGB = %.1f Mg/ha\\n",
            nrow(sweden), mean(sweden$AGB_T_HA, na.rm = TRUE)))
#> Sweden: 2619 cells, mean AGB = 56.0 Mg/ha\n
```
