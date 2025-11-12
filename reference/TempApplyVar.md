# Apply temporal adjustment and compute temporal variance in plot biomass

This function adjusts plot biomass values to a specified map year using
growth-rate increments and computes the temporal variance (standard
deviation) of biomass changes based on growth-rate standard deviations.
It combines the functionality of
[`TempApply`](https://atnt.github.io/Plot2Map/reference/TempApply.md)
and [`TempVar`](https://atnt.github.io/Plot2Map/reference/TempVar.md)
into a single call.

## Usage

``` r
TempApplyVar(plt, map_year, gez = "all", gr = NULL, sds = NULL)
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

- gr:

  Optional data.frame of growth rates containing columns `GEZ`, `ZONE`,
  `FAO.ecozone`, `GR1`, `GR2`, `GR3`. If `NULL`, default data from
  Plot2Map is used.

- sds:

  Optional data.frame of growth rate standard deviations containing
  columns `GEZ`, `ZONE`, `FAO.ecozone`, `SD1`, `SD2`, `SD3`. If `NULL`,
  default data from Plot2Map is used.

## Value

A data.frame with the following columns:

- `AGB_T_HA`:

  Adjusted biomass values for the specified `map_year`.

- `AGB_T_HA_ORIG`:

  Original biomass values before adjustment.

- `sdGrowth`:

  Temporal standard deviation of the biomass adjustment.

- Additional:

  All other columns from the input `plt`.

## Examples

``` r
library(Plot2Map)
set.seed(42)
sample_plots <- plots[sample(nrow(plots), 10), ]
sample_plots_gez <- BiomePair(sample_plots)
result <- TempApplyVar(sample_plots_gez, map_year = 2004)
head(result)
#>   PLOT_ID AGB_T_HA AVG_YEAR SIZE_HA   POINT_X  POINT_Y   ZONE
#> 1     EU2 97.17177     2001   0.196  1.305915 42.59214 Europe
#> 2     EU2 62.49786     2000   0.196 -3.988935 40.33963 Europe
#> 3     EU2 57.64102     2003   0.196 -2.707056 42.68534 Europe
#> 4     EU1 63.39700     2008   0.015 15.218726 59.81792 Europe
#> 5     EU2 82.59592     2007   0.196 -7.182390 37.34714 Europe
#> 6     EU2 33.56841     2007   0.196 -7.216243 37.34765 Europe
#>                   FAO.ecozone         GEZ AGB_T_HA_ORIG sdGrowth
#> 1   Temperate mountain system   Temperate      87.87177      9.3
#> 2      Subtropical dry forest Subtropical      46.49786     16.0
#> 3 Subtropical mountain system Subtropical      55.14102      2.5
#> 4    Boreal coniferous forest      Boreal      67.79700      4.4
#> 5      Subtropical dry forest Subtropical      94.59592     12.0
#> 6      Subtropical dry forest Subtropical      45.56841     12.0
```
