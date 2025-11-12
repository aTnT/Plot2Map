# Sample block mean mapped forest cover over a region of interest

This function samples the block mean mapped forest cover over a given
polygon. It can use either a custom forest cover mask provided as input
or download and use Global Forest Change (GFC) tree cover tiles (Hansen
et al., 2013).

## Usage

``` r
sampleTreeCover(
  roi,
  thresholds,
  forest_mask = NULL,
  weighted_mean = FALSE,
  gfc_folder = "data/GFC",
  gfc_dataset_year = "latest"
)
```

## Arguments

- roi:

  An sf or SpatVector object representing the Region of Interest.

- thresholds:

  Numeric vector of tree cover thresholds percentages (e.g., c(10, 20,
  30)) to calculate forest cover percentages.

- forest_mask:

  A SpatRaster object with a custom forest cover mask. If NULL, Hansen
  GFC tree cover tiles will be downloaded and used.

- weighted_mean:

  Logical, if TRUE the weighted mean is calculated considering the
  approximate fraction of each cell that is covered by the roi (default
  is FALSE).

- gfc_folder:

  Character string specifying the directory to download GFC data.

- gfc_dataset_year:

  Numeric value describing which version of the Hansen data to use: any
  year in the 2018-2023 range or "latest" (default).

## Value

Numeric value representing the mean forest cover for the polygon.

## References

M. C. Hansen et al., High-Resolution Global Maps of 21st-Century Forest
Cover Change. Science342,850-853(2013).
[DOI:10.1126/science.1244693](https://doi.org/10.1126/science.1244693)

## Examples

``` r
# Load required libraries
library(sf)

# Define a region of interest (ROI) in the Daintree forest
roi_daintree <- st_polygon(list(rbind(c(145.3833, -16.2500), c(145.3933, -16.2500),
                                      c(145.3933, -16.2400), c(145.3833, -16.2400),
                                      c(145.3833, -16.2500))))
roi_sf_daintree <- st_sfc(roi_daintree, crs = 4326)

# Example 1: Calculate forest cover (unweighted)
if (FALSE) { # \dontrun{
sampleTreeCover(roi_sf_daintree, thresholds = c(10, 20, 30))
} # }

# Example 2: Calculate forest cover (weighted)
if (FALSE) { # \dontrun{
sampleTreeCover(roi_sf_daintree, thresholds = c(10, 20, 30), weighted_mean = TRUE)
} # }
```
