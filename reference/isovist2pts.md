# Create isovists using two points

Create one or more isovists at particular points, given another point
for direction and an angle for field of view

## Usage

``` r
isovist2pts(boundaryMap, x, y, toX, toY, viewAngle, verbose = FALSE)
```

## Arguments

- boundaryMap:

  A ShapeMap with lines designating the isovist boundaries

- x:

  X coordinate of the origin points

- y:

  Y coordinate of the origin points

- toX:

  X coordinate of the target points

- toY:

  Y coordinate of the target points

- viewAngle:

  The angle signifying the isovist's field of view

- verbose:

  Optional. Show more information of the process.

## Value

A ShapeMap with the isovist polygons

## Examples

``` r
mifFile <- system.file(
    "extdata", "testdata", "simple",
    "simple_interior.mif",
    package = "alcyon"
  )
  sfMap <- st_read(mifFile,
    geometry_column = 1L, quiet = TRUE
  )
  shapeMap <- as(sfMap[, vector()], "ShapeMap")
isovist2pts(
  shapeMap,
  x = c(3.01, 1.3),
  y = c(6.70, 5.2),
  toX = c(3.40, 1.1),
  toY = c(6.50, 5.6),
  viewAngle = 3.14,
  FALSE
)
#> Simple feature collection with 2 features and 9 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 1.3 ymin: 1.544019 xmax: 7.89713 ymax: 6.908548
#> CRS:           NA
#>   Isovist Area Isovist Compactness Isovist Drift Angle Isovist Drift Magnitude
#> 1   21.3243446           0.4878067           300.27298               2.6501689
#> 2    0.4160788           0.6541165            59.55943               0.7745971
#>   Isovist Max Radial Isovist Min Radial Isovist Occlusivity Isovist Perimeter
#> 1           6.002915                  0            5.059755         23.437910
#> 2           1.807116                  0            1.413628          2.827256
#>   Ref                       geometry
#> 1   0 POLYGON ((7.8825 6.908548, ...
#> 2   1 POLYGON ((1.888668 6.908548...
```
