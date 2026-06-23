# One-to-one traversal

Runs one-to-one traversal on a map with a graph. This is applicable to:

- LatticeMaps (Visibility Graph Analysis)

- Segment ShapeGraphs (Segment analysis)

## Usage

``` r
oneToOneTraverse(
  map,
  traversalType,
  fromX,
  fromY,
  toX,
  toY,
  quantizationWidth = NA,
  copyMap = TRUE,
  verbose = FALSE
)
```

## Arguments

- map:

  A LatticeMap or Segment ShapeGraph

- traversalType:

  The traversal type. See
  [TraversalType](https://spatialnous.github.io/alcyon/reference/TraversalType.md)

- fromX:

  X coordinate of the point(s) to start the traversal from

- fromY:

  X coordinate of the point(s) to start the traversal from

- toX:

  X coordinate of the point(s) to start the traversal from

- toY:

  X coordinate of the point(s) to start the traversal from

- quantizationWidth:

  Set this to use chunks of this width instead of continuous values for
  the cost of traversal. This is equivalent to the "tulip bins" for
  depthmapX's tulip analysis (1024 tulip bins = pi/1024
  quantizationWidth). Only works for Segment ShapeGraphs

- copyMap:

  Optional. Copy the internal sala map

- verbose:

  Optional. Show more information of the process.

## Value

Returns a list with:

- completed: Whether the analysis completed

- newAttributes: The new attributes that were created during the process

## Examples

``` r
# LatticeMap analysis (VGA)
mifFile <- system.file(
    "extdata", "testdata", "simple",
    "simple_interior.mif",
    package = "alcyon"
  )
  sfMap <- st_read(mifFile,
    geometry_column = 1L, quiet = TRUE
  )
  latticeMap <- makeVGALatticeMap(
    sfMap,
    gridSize = 0.5,
    fillX = 3.0,
    fillY = 6.0,
    maxVisibility = NA,
    boundaryGraph = FALSE,
    verbose = FALSE
  )
oneToOneTraverse(
  latticeMap,
  traversalType = TraversalType$Metric,
  fromX = 7.52,
  fromY = 6.02,
  toX = 5.78,
  toY = 2.96
)
#> stars object with 2 dimensions and 14 attributes
#> attribute(s):
#> Warning: number of columns of result is not a multiple of vector length (arg 1)
#>                                        Min.       1st Qu.        Median
#> Ref                               0.0000000  1.966108e+05 393221.500000
#> Connectivity                     39.0000000  6.500000e+01     75.000000
#> Point First Moment               92.0380707  1.451355e+02    173.266983
#> Point Second Moment             240.0000000  3.885625e+02    531.500000
#> blocked                           0.0000000  0.000000e+00      0.000000
#> contextfilled                     0.0000000  0.000000e+00      0.000000
#> filled                            0.0000000  0.000000e+00      1.000000
#> Metric Shortest Path             -1.0000000 -1.000000e+00     -1.000000
#> Metric Shortest Path Distance     0.0000000  7.017767e+00      9.964725
#> Metric Shortest Path Inv Me...    0.2380067  3.333333e-01      0.472136
#> Metric Shortest Path Linked      -1.0000000 -1.000000e+00     -1.000000
#> Metric Shortest Path Metric...    0.0000000  5.000000e-01      1.118034
#> Metric Shortest Path Order       -1.0000000 -1.000000e+00     -1.000000
#> Metric Shortest Path Visual...    0.0000000  1.000000e+00      1.000000
#>                                          Mean       3rd Qu.         Max. NAs
#> Ref                              3.932215e+05  5.898322e+05 7.864430e+05   0
#> Connectivity                     7.106667e+01  8.100000e+01 8.800000e+01  66
#> Point First Moment               1.750389e+02  2.021131e+02 2.921885e+02  66
#> Point Second Moment              5.387111e+02  6.655625e+02 1.157750e+03  66
#> blocked                          3.205128e-01  1.000000e+00 1.000000e+00   0
#> contextfilled                    0.000000e+00  0.000000e+00 0.000000e+00   0
#> filled                           5.769231e-01  1.000000e+00 1.000000e+00   0
#> Metric Shortest Path             2.777778e-01 -1.000000e+00 1.500000e+01  66
#> Metric Shortest Path Distance    8.800187e+00  1.144408e+01 1.416942e+01  66
#> Metric Shortest Path Inv Me...   5.325789e-01  6.666667e-01 1.000000e+00  66
#> Metric Shortest Path Linked     -9.555556e-01 -1.000000e+00 0.000000e+00  66
#> Metric Shortest Path Metric...   1.235573e+00  2.000000e+00 3.201562e+00  66
#> Metric Shortest Path Order      -7.888889e-01 -1.000000e+00 5.000000e+00  66
#> Metric Shortest Path Visual...   2.066667e+00  3.000000e+00 9.000000e+00  66
#>                                 NA's
#> Ref                                0
#> Connectivity                       0
#> Point First Moment                 0
#> Point Second Moment                0
#> blocked                            0
#> contextfilled                      0
#> filled                             0
#> Metric Shortest Path               0
#> Metric Shortest Path Distance      0
#> Metric Shortest Path Inv Me...     0
#> Metric Shortest Path Linked        0
#> Metric Shortest Path Metric...     0
#> Metric Shortest Path Order         0
#> Metric Shortest Path Visual...     0
#> dimension(s):
#>   from to offset delta x/y
#> x    1 13   1.75   0.5 [x]
#> y    1 12   7.25  -0.5 [y]

# Segment analysis
mifFile <- system.file(
    "extdata", "testdata", "barnsbury",
    "barnsbury_small_segment_original.mif",
    package = "alcyon"
  )
  sfMap <- st_read(mifFile,
    geometry_column = 1L, quiet = TRUE
  )
  shapeGraph <- as(sfMap, "SegmentShapeGraph")
oneToOneTraverse(
  shapeGraph,
  traversalType = TraversalType$Topological,
  fromX = 1217.1,
  fromY = -1977.3,
  toX = 1017.8,
  toY = -1699.3
)
#> Simple feature collection with 173 features and 12 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 989.1063 ymin: -2040.514 xmax: 1377.667 ymax: -1535.739
#> CRS:           NA
#> First 10 features:
#>    Depthmap_Ref Angular_Connectivity Axial_Line_Ref Connectivity Data_Map_Ref
#> 1             0             1.981438             -1            3            0
#> 2             1             2.115670             -1            4            1
#> 3             2             1.775849             -1            4            2
#> 4             3             2.034886             -1            4            3
#> 5             4             2.002752             -1            3            4
#> 6             5             2.052529             -1            3            5
#> 7             6             2.049290             -1            4            6
#> 8             7             2.035002             -1            4            7
#> 9             8             2.948040             -1            5            8
#> 10            9             4.000000             -1            6            9
#>    Segment_Length                       geometry Angular Connectivity
#> 1       65.799698 LINESTRING (989.1063 -1681....             1.981438
#> 2       77.203362 LINESTRING (1049.228 -1654....             2.115669
#> 3       90.154808 LINESTRING (1119.769 -1623....             1.775849
#> 4       91.088219 LINESTRING (1202.144 -1586....             2.034886
#> 5       35.005592 LINESTRING (1285.372 -1549....             2.002752
#> 6       69.541687 LINESTRING (1317.357 -1535....             2.052529
#> 7       37.176987 LINESTRING (1336.921 -1602....             2.049290
#> 8       35.278736 LINESTRING (1347.381 -1638....             2.035002
#> 9       34.014023 LINESTRING (1357.306 -1672....             2.948040
#> 10       9.775865 LINESTRING (1366.875 -1704....             4.000000
#>    Axial Line Ref Data Map Ref Segment Length Topological Shortest Path Depth
#> 1              -1            0      65.799698                              -1
#> 2              -1            1      77.203362                              -1
#> 3              -1            2      90.154808                              -1
#> 4              -1            3      91.088219                              -1
#> 5              -1            4      35.005592                              -1
#> 6              -1            5      69.541687                              -1
#> 7              -1            6      37.176987                               0
#> 8              -1            7      35.278736                              -1
#> 9              -1            8      34.014023                              -1
#> 10             -1            9       9.775865                              -1
#>    Topological Shortest Path Order
#> 1                               -1
#> 2                               -1
#> 3                               -1
#> 4                               -1
#> 5                               -1
#> 6                               -1
#> 7                               40
#> 8                               -1
#> 9                               -1
#> 10                              -1
```
