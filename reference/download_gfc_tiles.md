# Download Global Forest Change (GFC) product tiles

Downloads the specified GFC product tiles from the Hansen et al. Global
Forest Change dataset.

## Usage

``` r
download_gfc_tiles(
  tiles,
  output_dir,
  images = c("treecover2000", "lossyear"),
  dataset = "GFC-2023-v1.11",
  timeout = 1000
)
```

## Arguments

- tiles:

  An sf object with GFC tiles information as returned by
  `calculate_gfc_tiles`

- output_dir:

  Directory where downloaded files will be saved

- images:

  Character vector specifying which image types to download. Options
  include 'treecover2000', 'lossyear', 'gain', 'datamask', 'first', and
  'last'

- dataset:

  Version of the Hansen GFC dataset to use (e.g., 'GFC-2023-v1.11')

- timeout:

  Download timeout in seconds

## Value

Character vector of paths to successfully downloaded files

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)

# Create a polygon in Central Africa
aoi <- st_sf(geometry = st_sfc(st_polygon(list(cbind(
  c(20, 21, 21, 20, 20),
  c(-1, -1, 1, 1, -1)))), crs = 4326))

# Calculate required tiles
tiles <- calculate_gfc_tiles(aoi)

# Download tree cover and loss year data
download_gfc_tiles(
  tiles,
  "data/GFC",
  images = c("treecover2000", "lossyear"),
  dataset = "GFC-2023-v1.11"
)
} # }
```
