# Check and covert plt input

This helper function checks if the input is an sf object or a dataframe,
and ensures it contains the required coordinate columns.

## Usage

``` r
check_and_convert_plt(plt, ez = FALSE)
```

## Arguments

- plt:

  A data frame or sf object containing plot data. For data frame input
  format, longitude and latitude coordinates should be placed under
  "POINT_X" and "POINT_Y" columns respectively in WGS 84 CRS.

## Value

A dataframe with "POINT_X" and "POINT_Y" columns

## Details

If the input is an sf object, it is converted to a dataframe with
coordinate columns. The function then checks for the presence of
"POINT_X" and "POINT_Y" columns.
