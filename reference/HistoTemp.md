# Visualize histogram of temporal fix effect

This function creates a histogram to visualize the effect of temporal
adjustment or forest fraction correction on AGB values. It generates a
plot comparing the distribution of AGB values before and after the
adjustment.

## Usage

``` r
HistoTemp(df, year, outDir = "results")
```

## Arguments

- df:

  A data frame containing AGB data.

- year:

  Numeric value indicating the year of analysis.

- outDir:

  Character string specifying the output directory for saving the
  histogram (default: "results").

## Value

Invisibly returns NULL. The function creates a plot as a side effect.

## Examples

``` r
set.seed(42)
sample_plots <- plots[sample(nrow(plots), 10), ]
sample_plots <- BiomePair(sample_plots)
sample_plots <- TempApply(sample_plots, 2004)
head(sample_plots)
#>   PLOT_ID AGB_T_HA AVG_YEAR SIZE_HA   POINT_X  POINT_Y   ZONE
#> 1     EU2 97.17177     2001   0.196  1.305915 42.59214 Europe
#> 2     EU2 62.49786     2000   0.196 -3.988935 40.33963 Europe
#> 3     EU2 57.64102     2003   0.196 -2.707056 42.68534 Europe
#> 4     EU1 63.39700     2008   0.015 15.218726 59.81792 Europe
#> 5     EU2 82.59592     2007   0.196 -7.182390 37.34714 Europe
#> 6     EU2 33.56841     2007   0.196 -7.216243 37.34765 Europe
#>                   FAO.ecozone         GEZ AGB_T_HA_ORIG
#> 1   Temperate mountain system   Temperate      87.87177
#> 2      Subtropical dry forest Subtropical      46.49786
#> 3 Subtropical mountain system Subtropical      55.14102
#> 4    Boreal coniferous forest      Boreal      67.79700
#> 5      Subtropical dry forest Subtropical      94.59592
#> 6      Subtropical dry forest Subtropical      45.56841
HistoTemp(sample_plots, 2004)
```
