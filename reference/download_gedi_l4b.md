# Download and process GEDI L4B gridded biomass data

Downloads, processes, and saves GEDI L4B Gridded Biomass data for a
specified region of interest. The function retrieves data via the ORNL
DAAC STAC API, filters for a specified band, creates a data cube, and
saves the result as a GeoTIFF.

## Usage

``` r
download_gedi_l4b(
  roi = NULL,
  gedi_l4b_folder = "data/GEDI_L4B/",
  collection = "GEDI_L4B_Gridded_Biomass_V2_1_2299_2.1",
  gedi_l4b_resolution = 0.001,
  gedi_l4b_band = "MU"
)
```

## Arguments

- roi:

  An sf object representing the region of interest. If NULL, the global
  extent of available GEDI L4B data is used.

- gedi_l4b_folder:

  Character, the folder to save the downloaded GeoTIFF file. Default is
  "data/GEDI_L4B/".

- collection:

  Character, the STAC collection ID for GEDI L4B data. Default is
  "GEDI_L4B_Gridded_Biomass_V2_1_2299_2.1".

- gedi_l4b_resolution:

  Numeric, the spatial resolution of the processed output GeoTIFF in
  degrees. The native resolution of the GEDI L4B gridded dataset is 1
  km, approximately 0.001 degrees at the equator. Default is 0.001.

- gedi_l4b_band:

  Character, the band to filter for. See options in the Details section
  below. Default is "MU".

## Value

Character, the file path to the processed GeoTIFF file.

## Details

This function requires a valid Earthdata Login account and the
corresponding credentials Ensure that you have set up your Earthdata
Login credentials by either:

- Setting the EDL_USER and EDL_PASS environment variables in a .Renviron
  file.

- Running
  `earthdatalogin::edl_netrc(username = EDL_USER, password = EDL_PASS)`
  with your Earthdata Login username and password.

The available bands are based on GEDI L4B Gridded Biomass Density
Version 2.1 documentation:

- "MU": Mean aboveground biomass density (MU): Estimated mean AGBD for
  the grid cell, including forest and non-forest

- "SE": Mean aboveground biomass density standard error (SE): Standard
  Error of the mean estimate, combining sampling and modeling
  uncertainty

- "PE": Standard error as a fraction of the estimated mean AGBD. If
  \>100%, the cell values are truncated to 100.

- "V1": Variance component 1 (V1)

- "V2": Variance component 2 (V2)

- "QF": Quality flag

- "NS": Number of samples

- "MI": Mode of interference

- "PS": Prediction stratum

- "NC": Number of clusters

Consult the reference below for more details about the bands.

## References

[Dubayah, R., et al. (2022). GEDI L4B Gridded Biomass Data, Version 2.1.
NASA Earthdata.](https://doi.org/10.3334/ORNLDAAC/2299)

## Examples

``` r
if (FALSE) { # \dontrun{
  # Example usage with a defined ROI:
  # Define ROI over the Amazon Basin
  roi <- sf::st_as_sf(data.frame(y = c(-10, 0), x = c(-70, -60)), coords = c("x", "y"), crs = 4326)
  # Downloads biomass data for the defined region of interest
  output_file <- download_gedi_l4b(roi = roi, gedi_l4b_band = "MU")
  print(output_file)
} # }
```
