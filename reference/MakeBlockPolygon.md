# Create a square polygon (block) based on input coordinates and size

This function creates a square polygon (block) that contains the input
coordinates. The block is aligned to a grid defined by the `size`
parameter, with the coordinates rounded to the nearest multiple of
`size`. The resulting polygon is returned as an `sf` object.

## Usage

``` r
MakeBlockPolygon(x, y, size)
```

## Arguments

- x:

  Numeric. The x-coordinate (e.g., longitude) of a point within the
  desired block.

- y:

  Numeric. The y-coordinate (e.g., latitude) of a point within the
  desired block.

- size:

  Numeric. The size (side length) of the square block in the same units
  as the input coordinates (e.g., degrees).

## Value

An `sf` object representing the square polygon. The polygon is created
in the WGS 84 coordinate reference system (EPSG:4326).

## Details

The function calculates the lower-left corner of the block by aligning
the input coordinates to a grid defined by the `size` parameter. For
example:

- If `x = 5.2`, `y = 10.7`, and `size = 1`, the lower-left corner will
  be at `(5, 10)`.

- The block will extend from `(5, 10)` to `(6, 11)`.

- If `x = 5.2`, `y = 10.7`, and `size = 0.1`, the lower-left corner will
  be at `(5.1, 10.6)`.

- The block will extend from `(5.1, 10.6)` to `(5.2, 10.7)`.

The polygon is created with vertices in the following order:

1.  Lower-left corner `(xll, yll)`

2.  Lower-right corner `(xll + size, yll)`

3.  Upper-right corner `(xll + size, yll + size)`

4.  Upper-left corner `(xll, yll + size)`

5.  Lower-left corner `(xll, yll)` (to close the polygon)

The resulting polygon is returned as an `sf` object with a WGS 84
(EPSG:4326) coordinate reference system.

## Examples

``` r
# Create a 1x1 degree block including the point (5.2, 10.7) aligned to a 1x1 degree grid:
block <- MakeBlockPolygon(5.2, 10.7, 1)
print(block)
#> Simple feature collection with 1 feature and 0 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 5 ymin: 10 xmax: 6 ymax: 11
#> Geodetic CRS:  WGS 84
#>                         geometry
#> 1 POLYGON ((5 10, 6 10, 6 11,...
plot(block)

# Create a 0.25x0.25 degree block including the point (5.2, 10.7) aligned to a 0.25x0.25 degree grid:
block2 <- MakeBlockPolygon(5.2, 10.7, 0.25)
print(block2)
#> Simple feature collection with 1 feature and 0 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 5 ymin: 10.5 xmax: 5.25 ymax: 10.75
#> Geodetic CRS:  WGS 84
#>                         geometry
#> 1 POLYGON ((5 10.5, 5.25 10.5...
sf::st_centroid(block2)
#> Simple feature collection with 1 feature and 0 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 5.125 ymin: 10.62501 xmax: 5.125 ymax: 10.62501
#> Geodetic CRS:  WGS 84
#>                 geometry
#> 1 POINT (5.125 10.62501)
```
