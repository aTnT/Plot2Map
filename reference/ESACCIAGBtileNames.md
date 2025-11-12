# Generate ESA-CCI AGB tile names

This function generates file names for ESA-CCI AGB tiles based on a
given polygon.

## Usage

``` r
ESACCIAGBtileNames(
  pol,
  esacci_biomass_year = "latest",
  esacci_biomass_version = "latest"
)
```

## Arguments

- pol:

  An sf or SpatVector object representing the polygon of interest.

- esacci_biomass_year:

  The ESA CCI BIOMASS AGB tiles year to use. Use either 2010, 2015,
  2016, 2017, 2018, 2019, 2020, 2021, 2022 or "latest" (default).

- esacci_biomass_version:

  The ESA CCI BIOMASS AGB tiles version to use. Use either "v2.0",
  "v3.0", "v4.0", "v5.0", "v5.01", "v6.0" or "latest" (default).

## Value

A character vector of unique file names for ESA-CCI AGB tiles.
