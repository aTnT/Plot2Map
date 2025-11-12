# Download Global 2010 Tree Cover 30m data

This function downloads the Global 2010 Tree Cover 30m data within a
specified region of interest from the GLAD lab at the University of
Maryland.

## Usage

``` r
download_glad_tcc_2010(
  roi = NULL,
  output_folder = "data/GLAD_TCC_2010",
  n_cores = 1,
  timeout = 1800
)
```

## Arguments

- roi:

  An sf object representing the region of interest. If NULL, the global
  extent is used.

- output_folder:

  Directory to save downloaded files. Default is "data/GLAD_TCC_2010".

- n_cores:

  Number of cores to use for parallel download. Default is 1.

- timeout:

  Number of seconds for reaching file download timeout. Default is 1800.

## Value

A character vector of downloaded file paths.

## References

[Hansen, M. C., Potapov, P. V., Moore, R., Hancher, M., Turubanova, S.
A., Tyukavina, A., ... & Townshend, J. R. G. (2013). High-resolution
global maps of 21st-century forest cover change. science, 342(6160),
850-853.](https://doi.org/10.1126/science.1244693)

## Examples

``` r
if (FALSE) { # \dontrun{
roi <- st_as_sf(data.frame(x = c(-70, -60), y = c(-10, 0)), coords = c("x", "y"), crs = 4326)
download_glad_tcc_2010(roi = roi)
} # }
```
