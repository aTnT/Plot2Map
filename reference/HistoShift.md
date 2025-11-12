# Analyze AGB shift in bins

This function analyzes the shift in AGB values across different bins
before and after temporal adjustment or forest fraction correction. It
generates a summary table of the changes in AGB distribution.

## Usage

``` r
HistoShift(df, year, outDir = "results")
```

## Arguments

- df:

  A data frame containing AGB data.

- year:

  Numeric value indicating the year of analysis.

- outDir:

  Character string specifying the output directory for saving the CSV
  file (default: "results").

## Value

A data frame summarizing the changes in AGB distribution across bins.

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
HistoShift(sample_plots, 2004)
#>   agb_Mgha_bins n_pre n_post agb_Mgha_pre agb_Mgha_post
#> 1       (20,40]     1      2     37.31206      35.44024
#> 2       (40,60]     5      3     51.90657      53.98887
#> 3       (60,80]     1      2     67.79700      62.94743
#> 4      (80,100]     2      2     91.23385      89.88385
#> 5     (140,160]     1      1    150.97321     150.97321
```
