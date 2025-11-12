# Sample block mean mapped AGB over a region of interest

This function samples the block mean mapped Above Ground Biomass (AGB)
over a given polygon. It can use either a custom AGB map provided as
input, download and use ESA CCI BIOMASS AGB tiles, or download and use
GEDI L4B Gridded Biomass data.

## Usage

``` r
sampleAGBmap(
  roi,
  weighted_mean = FALSE,
  agb_raster = NULL,
  dataset = "custom",
  esacci_biomass_year = "latest",
  esacci_biomass_version = "latest",
  esacci_folder = "data/ESACCI-BIOMASS",
  gedi_l4b_folder = "data/GEDI_L4B/",
  gedi_l4b_band = "MU",
  gedi_l4b_resolution = 0.001,
  n_cores = 1,
  timeout = 600
)
```

## Arguments

- roi:

  An sf or SpatVector object representing the Region of Interest.

- weighted_mean:

  Logical, if TRUE the weighted mean is calculated considering the
  approximate fraction of each cell that is covered by the roi (default
  is FALSE).

- agb_raster:

  A SpatRaster object with the custom AGB map. If NULL, either ESA CCI
  BIOMASS AGB tiles or GEDI L4B data will be downloaded and used.

- dataset:

  Character, the dataset to use for AGB estimation. Options are
  "custom", "esacci", or "gedi". Default is "custom".

- esacci_biomass_year:

  The ESA CCI BIOMASS AGB tiles year to use. Use either 2010, 2015,
  2016, 2017, 2018, 2019, 2020, 2021, 2022 or "latest" (default).

- esacci_biomass_version:

  The ESA CCI BIOMASS AGB tiles version to use. Use either "v2.0",
  "v3.0", "v4.0", "v5.0", "v5.01", "v6.0" or "latest" (default).

- esacci_folder:

  Directory to save downloaded ESA CCI BIOMASS AGB files. Default is the
  relative path "data/ESACCI-BIOMASS".

- gedi_l4b_folder:

  Character, the folder to save the downloaded GeoTIFF file. Default is
  "data/GEDI_L4B/".

- gedi_l4b_band:

  Character, the band to filter for. See options in the Details section
  below. Default is "MU".

- gedi_l4b_resolution:

  Numeric, the spatial resolution of the processed output GeoTIFF in
  degrees. The native resolution of the GEDI L4B gridded dataset is 1
  km, approximately 0.001 degrees at the equator. Default is 0.001.

- n_cores:

  Number of cores to use for parallel download.

- timeout:

  Number of seconds for reaching file download timeout.

## Value

Numeric value representing the mean AGB for the polygon.

## References

[Santoro, M.; Cartus, O. (2024): ESA Biomass Climate Change Initiative
(Biomass_cci): Global datasets of forest above-ground biomass for the
years 2010, 2015, 2016, 2017, 2018, 2019, 2020 and 2021, v5.01. NERC EDS
Centre for Environmental Data Analysis, 22 August
2024.](https://dx.doi.org/10.5285/bf535053562141c6bb7ad831f5998d77)

[Dubayah, R., et al. (2022). GEDI L4B Gridded Biomass Data, Version 2.1.
NASA Earthdata.](https://doi.org/10.3334/ORNLDAAC/2299)

## Examples

``` r
# Load required libraries
library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE

# Define a region of interest (ROI) in the Congo Rainforest
roi_congo <- st_polygon(list(rbind(
  c(25.0089, 0.4735), c(25.0189, 0.4735),
  c(25.0189, 0.4835), c(25.0089, 0.4835),
  c(25.0089, 0.4735)
)))
roi_sf_congo <- st_sfc(roi_congo, crs = 4326)

# Example 1: Calculate mean AGB for the Congo ROI using ESA CCI BIOMASS (unweighted)
if (FALSE) { # \dontrun{
sampleAGBmap(roi_sf_congo, dataset = "esacci")
} # }

# Example 2: Calculate mean AGB for the Congo ROI using GEDI L4B (weighted)
if (FALSE) { # \dontrun{
sampleAGBmap(roi_sf_congo, dataset = "gedi", weighted_mean = TRUE)
} # }
```
