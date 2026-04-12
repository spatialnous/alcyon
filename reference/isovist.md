# Create isovists at point and direction angle

Create one or more isovists at particular points, given angle and field
of view

## Usage

``` r
isovist(boundaryMap, x, y, angle = NA, viewAngle = NA, verbose = FALSE)
```

## Arguments

- boundaryMap:

  A ShapeMap with lines designating the isovist boundaries

- x:

  X coordinate of the origin points

- y:

  Y coordinate of the origin points

- angle:

  The angle (from the X axis) of the isovist look direction

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
isovist(
  shapeMap,
  x = c(3.01, 1.3),
  y = c(6.70, 5.2),
  angle = 0.01,
  viewAngle = 3.14,
  FALSE
)
#> Simple feature collection with 2 features and 9 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 1.888668 ymin: 1.544019 xmax: 7.89713 ymax: 6.908548
#> CRS:           NA
#>    Isovist Area Isovist Compactness Isovist Drift Angle Isovist Drift Magnitude
#> 1  1.665612e+01        4.293649e-01            311.8469                2.771847
#> 2 -1.776357e-15       -1.951463e-16            253.0725                9.616652
#>   Isovist Max Radial Isovist Min Radial Isovist Occlusivity Isovist Perimeter
#> 1           6.002915          0.0000000            7.716775          22.07896
#> 2           3.686368          0.5886679            5.347612          10.69522
#>   Ref                       geometry
#> 1   0 POLYGON ((7.8825 6.908548, ...
#> 2   1 POLYGON ((1.888668 6.908548...
```
