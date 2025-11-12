# Get path to example sample file

This package comes bundled with a number of sample files in its
`inst/extdata` directory. This function make them easy to access

## Usage

``` r
sample_file(file = NULL)
```

## Arguments

- file:

  Name of file. If `NULL`, all sample files will be listed.

## Examples

``` r
if (FALSE) { # \dontrun{
sample_file()
sample_file("SampleUnformattedPlots.csv")
} # }
```
