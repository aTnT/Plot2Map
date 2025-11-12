# Compute tree cover forest mask for a specific year

This function computes a tree cover forest mask layer for a given year
based on a 2000 tree cover baseline - the Hansen Global Forest Change
(GFC) dataset or a 2010 tree cover baseline - the GLAD 2010 Tree Canopy
Cover dataset.

## Usage

``` r
compute_treecover(
  year,
  gfc_folder = "data/GFC",
  treecover_threshold = NULL,
  coords = NULL,
  output_folder = "data/treecover_calc",
  num_cores = 1,
  baseline = 2010
)
```

## Arguments

- year:

  Integer. The year for which to compute the tree cover (2001-2023).

- gfc_folder:

  Character. Path to the folder containing GFC dataset files. Default is
  "data/GFC".

- treecover_threshold:

  Numeric. Optional threshold for tree cover (0-100) at baseline year.
  If provided, only areas with tree cover values \>= this threshold will
  be considered. Default is NULL (no threshold).

- coords:

  Character. Coordinates of the tile to process, e.g., "10N_020E". If
  NULL, processes all available tiles.

- output_folder:

  Character. Path to the folder where output rasters should be saved.

- num_cores:

  Integer. Number of cores to use for parallel processing.

- baseline:

  Integer. Baseline year for tree cover dataset. Can be 2010 (default)
  or 2000.

## Value

A list. If coords is specified, only one tile is processed and returns a
file path to a single raster. Otherwise, returns a set of file paths,
each representing the tree cover at baseline for the specified year for
different tiles. The rasters are computed taking the tree cover values
from the baseline rasters, removing pixels where forest loss was
recorded at or before the provided year. If a tree cover threshold is
provided, all tree cover pixels below the threshold are set to NA.
Rasters are saved as a GeoTIFF file saved in the output_folder.

## Details

This function computes a tree cover forest mask by leveraging a baseline
tree cover and forest loss data. It uses two primary types of input
files:

1.  Tree cover data, which serves as the baseline:

    - For the 2000 baseline, the function uses files from the Hansen
      dataset for tree cover.

    - For the 2010 baseline, the function uses GLAD tree cover files.

    - Tree cover values represent the percentage of tree cover (0-100\\

2.  Forest loss year data:

    - The function uses Hansen Global Forest Change lossyear files.

    - Values in these files indicate the year of forest loss (1-23,
      corresponding to years 2001-2023), or 0 if no loss occurred.

The function applies the following steps:

1.  It reads the tree cover and forest loss rasters for the specified
    year and location.

2.  If a `treecover_threshold` is provided, all tree cover pixels with
    values below the threshold are set to NA.

3.  It removes pixels where forest loss was recorded at or before the
    specified year.

4.  The resulting raster represents a tree cover forest mask for the
    specified year, with areas of low tree cover or forest loss masked
    out.

5.  The function saves the resulting raster as a GeoTIFF file in the
    `output_folder`.

## References

Hansen, M.C., Potapov, P.V., Moore, R., Hancher, M., Turubanova, S.A.,
Tyukavina, A., Thau, D., Stehman, S.V., Goetz, S.J., Loveland, T.R.,
Kommareddy, A., Egorov, A., Chini, L., Justice, C.O., and Townshend,
J.R.G., 2013, High-Resolution Global Maps of 21st-Century Forest Cover
Change: Science, v. 342, no. 6160, p. 850-853, at
http://www.sciencemag.org/content/342/6160/850.abstract.

## Examples

``` r
if (FALSE) { # \dontrun{
# Compute all tiles tree cover for 2015 using 2010 baseline
treecover_2015 <- compute_treecover(2015)

# Compute 30% thresholded tree cover for the 10N, 20E tile for 2005 using
# 2000 tree cover baseline
treecover_2020 <- compute_treecover(2020,
                                    treecover_threshold = 30,
                                    coords = "10N_020E",
                                    output_folder = "data/treecover_calc",
                                    baseline = 2000)
} # }
```
