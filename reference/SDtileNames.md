# Generate AGB SD tile names

This function generates file names for AGB SD tiles based on a given
polygon.

## Usage

``` r
SDtileNames(
  pol,
  agbTilesDir = "data/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2020-fv5.0"
)
```

## Arguments

- pol:

  An sf or SpatVector object representing the polygon of interest.

## Value

A character vector of unique file names for AGB SD tiles.
