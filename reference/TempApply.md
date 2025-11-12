# Apply temporal adjustment to plot biomass

This function adjusts plot biomass values to align with the map year by
adding or subtracting annual growth increment to older or newer plots.
It uses growth data from a model-based estimate of growth-age
relationships.

## Usage

``` r
TempApply(plt, map_year, gez = "all")
```

## Arguments

- plt:

  A data frame or sf object containing plot data. For data frame input
  format, longitude and latitude coordinates should be placed under
  "POINT_X" and "POINT_Y" columns respectively in WGS 84 CRS.

- map_year:

  Numeric value indicating the AGB map year.

- gez:

  Character string specifying the Global Ecological Zones (GEZ) of
  interest to apply. If "all" (default), all GEZ in plt will be
  calculated.

## Value

A data frame with adjusted AGB values.

## Examples

``` r
set.seed(42)
sample_plots <- plots[sample(nrow(plots), 10), ]
sample_plots
#>      PLOT_ID     POINT_X   POINT_Y  AGB_T_HA AVG_YEAR SIZE_HA
#> 2369     EU2   1.3059145  42.59214  87.87177     2001   0.196
#> 5273     EU2  -0.1585048  42.63245 150.97321     2004   0.196
#> 1252     EU2  -1.0662342  39.61600  58.63558     2006   0.196
#> 356      EU2  -5.1210978  40.02713  37.31206     2004   0.196
#> 7700     EU1  15.2187262  59.81792  67.79700     2008   0.015
#> 3954     EU2  -7.1823899  37.34714  94.59592     2007   0.196
#> 5403     EU2  -7.2162433  37.34765  45.56841     2007   0.196
#> 932      EU2  -2.7070556  42.68534  55.14102     2003   0.196
#> 5637    AUS1 145.8683679 -20.69546  53.69000     2004   0.160
#> 4002     EU2  -3.9889352  40.33963  46.49786     2000   0.196

sample_plots_gez <- BiomePair(sample_plots)
sample_plots_gez
#>    PLOT_ID  AGB_T_HA AVG_YEAR SIZE_HA     POINT_X   POINT_Y      ZONE
#> 1     AUS1  53.69000     2004   0.160 145.8683679 -20.69546 Australia
#> 2      EU1  67.79700     2008   0.015  15.2187262  59.81792    Europe
#> 3      EU2  87.87177     2001   0.196   1.3059145  42.59214    Europe
#> 4      EU2  94.59592     2007   0.196  -7.1823899  37.34714    Europe
#> 5      EU2  45.56841     2007   0.196  -7.2162433  37.34765    Europe
#> 6      EU2  58.63558     2006   0.196  -1.0662342  39.61600    Europe
#> 7      EU2  37.31206     2004   0.196  -5.1210978  40.02713    Europe
#> 8      EU2  46.49786     2000   0.196  -3.9889352  40.33963    Europe
#> 9      EU2 150.97321     2004   0.196  -0.1585048  42.63245    Europe
#> 10     EU2  55.14102     2003   0.196  -2.7070556  42.68534    Europe
#>                    FAO.ecozone         GEZ
#> 1           Tropical shrubland    Tropical
#> 2     Boreal coniferous forest      Boreal
#> 3    Temperate mountain system   Temperate
#> 4       Subtropical dry forest Subtropical
#> 5       Subtropical dry forest Subtropical
#> 6       Subtropical dry forest Subtropical
#> 7       Subtropical dry forest Subtropical
#> 8       Subtropical dry forest Subtropical
#> 9    Temperate mountain system   Temperate
#> 10 Subtropical mountain system Subtropical

resultApply <- TempApply(sample_plots_gez, 2004)
resultApply
#>    PLOT_ID  AGB_T_HA AVG_YEAR SIZE_HA     POINT_X   POINT_Y      ZONE
#> 1      EU2  97.17177     2001   0.196   1.3059145  42.59214    Europe
#> 2      EU2  62.49786     2000   0.196  -3.9889352  40.33963    Europe
#> 3      EU2  57.64102     2003   0.196  -2.7070556  42.68534    Europe
#> 4      EU1  63.39700     2008   0.015  15.2187262  59.81792    Europe
#> 5      EU2  82.59592     2007   0.196  -7.1823899  37.34714    Europe
#> 6      EU2  33.56841     2007   0.196  -7.2162433  37.34765    Europe
#> 7      EU2  50.63558     2006   0.196  -1.0662342  39.61600    Europe
#> 8     AUS1  53.69000     2004   0.160 145.8683679 -20.69546 Australia
#> 9      EU2  37.31206     2004   0.196  -5.1210978  40.02713    Europe
#> 10     EU2 150.97321     2004   0.196  -0.1585048  42.63245    Europe
#>                    FAO.ecozone         GEZ AGB_T_HA_ORIG
#> 1    Temperate mountain system   Temperate      87.87177
#> 2       Subtropical dry forest Subtropical      46.49786
#> 3  Subtropical mountain system Subtropical      55.14102
#> 4     Boreal coniferous forest      Boreal      67.79700
#> 5       Subtropical dry forest Subtropical      94.59592
#> 6       Subtropical dry forest Subtropical      45.56841
#> 7       Subtropical dry forest Subtropical      58.63558
#> 8           Tropical shrubland    Tropical      53.69000
#> 9       Subtropical dry forest Subtropical      37.31206
#> 10   Temperate mountain system   Temperate     150.97321

resultVar <- TempVar(sample_plots_gez, 2004)
resultVar
#>    PLOT_ID  AGB_T_HA AVG_YEAR SIZE_HA     POINT_X   POINT_Y      ZONE
#> 1      EU2  98.67177     2001   0.196   1.3059145  42.59214    Europe
#> 2      EU2  58.49786     2000   0.196  -3.9889352  40.33963    Europe
#> 3      EU2  57.01602     2003   0.196  -2.7070556  42.68534    Europe
#> 4      EU1  67.79700     2008   0.015  15.2187262  59.81792    Europe
#> 5      EU2  85.59592     2007   0.196  -7.1823899  37.34714    Europe
#> 6      EU2  36.56841     2007   0.196  -7.2162433  37.34765    Europe
#> 7      EU2  52.63558     2006   0.196  -1.0662342  39.61600    Europe
#> 8     AUS1  53.69000     2004   0.160 145.8683679 -20.69546 Australia
#> 9      EU2  37.31206     2004   0.196  -5.1210978  40.02713    Europe
#> 10     EU2 150.97321     2004   0.196  -0.1585048  42.63245    Europe
#>                    FAO.ecozone         GEZ sdGrowth
#> 1    Temperate mountain system   Temperate   10.800
#> 2       Subtropical dry forest Subtropical   12.000
#> 3  Subtropical mountain system Subtropical    1.875
#> 4     Boreal coniferous forest      Boreal    0.000
#> 5       Subtropical dry forest Subtropical    9.000
#> 6       Subtropical dry forest Subtropical    9.000
#> 7       Subtropical dry forest Subtropical    6.000
#> 8           Tropical shrubland    Tropical    0.000
#> 9       Subtropical dry forest Subtropical    0.000
#> 10   Temperate mountain system   Temperate    0.000
```
