# Determine plot data type from structure and columns

This helper function identifies the type of plot data based on its
structure and available columns. It performs a series of checks in the
following priority order:

## Usage

``` r
determineDataType(plot_data)
```

## Arguments

- plot_data:

  A data frame containing plot data

## Value

Character string indicating the plot data type: "tree_level", "point",
"polygon", "nested", or "lidar"

## Details

1.  Checks if data is an sf object (classifies as "polygon")

2.  Checks for column names indicating nested plot structure

3.  Checks for complex IDs or GUIDs in ID columns that suggest nested
    structure

4.  Checks for processed tree-level data with AGB and standard deviation

5.  Checks for raw tree data with standard column names

6.  Checks for non-standard tree data based on column name patterns

7.  Checks for LiDAR indicators like "cv" or "raster" columns

8.  Defaults to "point" type if no other type is detected
