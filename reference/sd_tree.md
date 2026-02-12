# Calculate AGB and standard deviations from tree-level data

This function calculates plot-level Above Ground Biomass (AGB) and
Standard Deviation using tree-level data and plot locations. It uses the
BIOMASS package for AGB calculation and standard deviation estimation.
The function automatically detects common column name variations and
applies taxonomic correction using BIOMASS::correctTaxo.

## Usage

``` r
sd_tree(plot, xy, region = "World", max_retries = 3, retry_delay = 1)
```

## Arguments

- plot:

  A data frame containing tree-level data. The function automatically
  detects column names with flexible patterns:

  ID column

  :   Detects: id, ID, code, CODE, plot, PLOT, plot_id, PLOT_ID, plotId,
      PlotId

  Diameter column

  :   Detects: diameter, dbh, DBH, D_idtree, d_idtree, dbh_allo,
      DBH_allo, diam, d, D

  Genus column

  :   Detects: Genus_accepted, genus_accepted (preferred), genus, Genus,
      GENUS

  Species column

  :   Detects: Species_accepted, species_accepted (preferred), species,
      Species, SPECIES, sp, SP

  Height column (optional)

  :   Detects: height, HEIGHT, H, h, h95, H95, HC, hc, tree_height,
      TREE_HEIGHT

- xy:

  A data frame containing plot location data. The function automatically
  detects column names:

  ID column

  :   Detects: id, ID, code, CODE, plot, PLOT, plot_id, PLOT_ID, plotId,
      PlotId

  X coordinate

  :   Detects: x, X, xutmr, XUTMR, utm_x, UTM_X, longitude, LONGITUDE,
      long, lon

  Y coordinate

  :   Detects: y, Y, yutmr, YUTMR, utm_y, UTM_Y, latitude, LATITUDE, lat

  size

  :   Plot size in square meters (numeric).

  year

  :   Year of measurement or survey (numeric).

- region:

  Region (or vector of region) of interest of your sample. By default,
  Region is set to 'World', but you can restrict the WD estimates to a
  single region :

  - `AfricaExtraTrop`: Africa (extra tropical)

  - `AfricaTrop`: Africa (tropical)

  - `Australia`: Australia

  - `AustraliaTrop`: Australia (tropical)

  - `CentralAmericaTrop`: Central America (tropical)

  - `China`: China

  - `Europe`: Europe

  - `India`: India

  - `Madagascar`: Madagascar

  - `Mexico`: Mexico

  - `NorthAmerica`: North America

  - `Oceania`: Oceania

  - `SouthEastAsia`: South-East Asia

  - `SouthEastAsiaTrop`: South-East Asia (tropical)

  - `SouthAmericaExtraTrop`: South America (extra tropical)

  - `SouthAmericaTrop`: South America (tropical)

  - `World`: World

- max_retries:

  Maximum number of retry attempts for taxonomic correction API calls
  (default: 3).

- retry_delay:

  Initial delay in seconds between retry attempts, with exponential
  backoff (default: 1).

## Value

A data frame with plot-level AGB estimates and standard deviations,
including columns:

- PLOT_ID:

  Unique identifier for each plot.

- POINT_X:

  X-coordinate of the plot location.

- POINT_Y:

  Y-coordinate of the plot location.

- SIZE_HA:

  Plot size in hectares.

- AVG_YEAR:

  Average year of measurement across trees in the plot.

- AGB_T_HA:

  Above Ground Biomass scaled to tons per hectare.

- sdTree:

  Standard deviation of AGB within the plot.

## Details

The function performs the following steps:

1.  Flexible column dDetection: automatically detects and maps column
    name variations to standard names. The function provides information
    about which columns are being used.

2.  Taxonomic correction: applies BIOMASS::correctTaxo() to standardize
    genus and species names, with preference for accepted taxonomic
    names (e.g., Genus_accepted over genus). Uses a retry mechanism to
    handle temporary API failures/delays.

3.  Filters trees with diameter \>= 10cm.

4.  Retrieves wood density data using corrected taxonomy.

5.  Computes or uses provided tree height data. When height data is
    missing and the brms package is unavailable, automatically detects
    Feldpausch regions using coordinates and
    BIOMASS::computeFeldRegion() for height-diameter relationships.

6.  Runs Monte Carlo simulation for AGB estimation.

7.  Calculates plot-level AGB and standard deviation.

8.  Scales values per hectare.

## Column Name Flexibility

This function resolves GitHub issue \#6 by providing robust column name
detection. It handles common variations found in real-world datasets,
including:

- Research databases with standardized column names (e.g., D_idtree,
  xutmr/yutmr)

- Field data with simple column names (e.g., dbh, x/y)

- Taxonomically corrected data with accepted names (e.g.,
  Genus_accepted/Species_accepted)

- Various coordinate systems (UTM, lat/lon, etc.)

## References

Feldpausch, T.R., et al. (2012). *Tree height integrated into
pantropical forest biomass estimates.* Biogeosciences, 9, 3381–3403.

## Examples

``` r
plotsTree <- utils::read.csv(sample_file("SampleTree.csv"))
head(plotsTree)
#>     id      genus    species  diameter  size                 fez      gez year
#> 1 BSP1 Terminalia  bellirica  3.501409 10000 tropical rainforest tropical 1996
#> 2 BSP1   Ziziphus   oenoplia  3.819719 10000 tropical rainforest tropical 1996
#> 3 BSP1    Aporosa lindleyana 16.870424 10000 tropical rainforest tropical 1996
#> 4 BSP1      Ixora  brachiata  4.138029 10000 tropical rainforest tropical 1996
#> 5 BSP1   Wrightia    arborea  5.411268 10000 tropical rainforest tropical 1996
#> 6 BSP1      Ixora  brachiata  3.183099 10000 tropical rainforest tropical 1996

xyTree <- utils::read.csv(sample_file("SampleTreeXY.csv"))
head(xyTree)
#>     id        y        x
#> 1 BSP1 14.36806 74.91944
#> 2 BSP1 14.36806 74.91944
#> 3 BSP1 14.36806 74.91944
#> 4 BSP1 14.36806 74.91944
#> 5 BSP1 14.36806 74.91944
#> 6 BSP1 14.36806 74.91944

plot_uncertainties <- sd_tree(plotsTree, xyTree, region = "India")
#> Using useCache=TRUE is recommended to reduce online search time for the next query
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Source iplant_tnrs:27712
#> Corrections FALSE:27092, TRUE:599, TaxaNotFound:19, SpNotFound:2
#> 
#> Warning: DRYAD data only stored 289 wood density values in your region of interest. You could provide additional wood densities (parameter addWoodDensityData) or widen your region (region="World")
#> The reference dataset contains 289 wood density values
#> Your taxonomic table contains 340 taxa
#> No tree height data found in original plot data. Calculating height using BIOMASS height-diameter model.
head(plot_uncertainties)
#>   PLOT_ID  POINT_X  POINT_Y SIZE_HA AVG_YEAR  AGB_T_HA    sdTree
#> 1    BSP1 74.91944 14.36806       1     1996 159.00181  9.933843
#> 2   BSP10 74.69861 14.94722       1     1996 417.99472 22.585124
#> 3  BSP100 75.82000 13.54722       1     1996  41.25254  3.934893
#> 4  BSP101 75.43444 13.72639       1     1996 349.38941 38.024094
#> 5  BSP102 74.88833 13.92361       1     1996 516.87190 39.705064
#> 6  BSP104 74.90917 13.87000       1     1996 700.53039 74.854013
```
