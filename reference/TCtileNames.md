# Generate tree cover tile names

This function generates file names for tree cover tiles based on a given
polygon and year.

## Usage

``` r
TCtileNames(pol, year = 2010, treeCoverDir = "data/treecover2010_v3_100m")
```

## Arguments

- pol:

  An sf or SpatVector object representing the polygon of interest.

- year:

  Numeric, the year for which to generate tile names (2010, 2015, or
  2020).

## Value

A character vector of unique file names for tree cover tiles.
