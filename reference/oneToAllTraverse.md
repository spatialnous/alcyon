# One-to-all traversal

Runs one-to-all traversal on a map with a graph. This is applicable to:

- LatticeMaps (Visibility Graph Analysis)

- Axial ShapeGraphs (Axial analysis)

- Segment ShapeGraphs (Segment analysis)

## Usage

``` r
oneToAllTraverse(
  map,
  traversalType,
  fromX,
  fromY,
  quantizationWidth = NA,
  copyMap = TRUE,
  verbose = FALSE
)
```

## Arguments

- map:

  A LatticeMap, Axial ShapeGraph or Segment ShapeGraph

- traversalType:

  The traversal type. See
  [TraversalType](https://spatialnous.github.io/alcyon/reference/TraversalType.md)

- fromX:

  X coordinate of the point to start the traversal from

- fromY:

  X coordinate of the point to start the traversal from

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
oneToAllTraverse(
  latticeMap,
  traversalType = TraversalType$Metric,
  fromX = 3.01,
  fromY = 6.7
)
#> stars object with 2 dimensions and 10 attributes
#> attribute(s):
#> Warning: number of columns of result is not a multiple of vector length (arg 1)
#>                                      Min.      1st Qu.       Median
#> Ref                               0.00000 1.966108e+05 3.932215e+05
#> Connectivity                     39.00000 6.500000e+01 7.500000e+01
#> Point First Moment               92.03807 1.451355e+02 1.732670e+02
#> Point Second Moment             240.00000 3.885625e+02 5.315000e+02
#> blocked                           0.00000 0.000000e+00 0.000000e+00
#> contextfilled                     0.00000 0.000000e+00 0.000000e+00
#> filled                            0.00000 0.000000e+00 1.000000e+00
#> Metric Step Shortest-Path A...    0.00000 0.000000e+00 0.000000e+00
#> Metric Step Shortest-Path L...    0.00000 1.852082e+00 3.041381e+00
#> Metric Straight-Line Distance     0.00000 1.852082e+00 3.041381e+00
#>                                         Mean      3rd Qu.         Max. NAs NA's
#> Ref                             3.932215e+05 5.898322e+05 7.864430e+05   0    0
#> Connectivity                    7.106667e+01 8.100000e+01 8.800000e+01  66    0
#> Point First Moment              1.750389e+02 2.021131e+02 2.921885e+02  66    0
#> Point Second Moment             5.387111e+02 6.655625e+02 1.157750e+03  66    0
#> blocked                         3.205128e-01 1.000000e+00 1.000000e+00   0    0
#> contextfilled                   0.000000e+00 0.000000e+00 0.000000e+00   0    0
#> filled                          5.769231e-01 1.000000e+00 1.000000e+00   0    0
#> Metric Step Shortest-Path A...  1.721897e-02 0.000000e+00 6.559583e-01  66    0
#> Metric Step Shortest-Path L...  2.940409e+00 4.032915e+00 5.408327e+00  66    0
#> Metric Straight-Line Distance   2.934198e+00 4.031129e+00 5.408327e+00  66    0
#> dimension(s):
#>   from to offset delta x/y
#> x    1 13   1.75   0.5 [x]
#> y    1 12   7.25  -0.5 [y]

# Axial analysis
mifFile <- system.file(
    "extdata", "testdata", "barnsbury",
    "barnsbury_small_axial_original.mif",
    package = "alcyon"
  )
  sfMap <- st_read(mifFile,
    geometry_column = 1L, quiet = TRUE
  )
  shapeGraph <- as(sfMap, "AxialShapeGraph")
oneToAllTraverse(
  shapeGraph,
  traversalType = TraversalType$Topological,
  fromX = 1217.1,
  fromY = -1977.3
)
#> Simple feature collection with 58 features and 7 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 946 ymin: -2177 xmax: 1537 ymax: -1386
#> CRS:           NA
#> First 10 features:
#>    Depthmap_Ref Connectivity Data_Map_Ref Line_Length
#> 1             0            6            0    396.1881
#> 2             1            7            1    259.4803
#> 3             2            7            2    244.4770
#> 4             3            8            3    210.3450
#> 5             4            4            4    375.4797
#> 6             5            8            5    145.4648
#> 7             6            6            6    596.2349
#> 8             7            6            7    205.1829
#> 9             8            7            8    385.0117
#> 10            9           10            9    273.1483
#>                          geometry Data Map Ref Line Length Step Depth
#> 1  LINESTRING (984 -1684, 1346...            0    396.1881          3
#> 2  LINESTRING (1306 -1497, 137...            1    259.4803          3
#> 3  LINESTRING (1257 -1772, 149...            2    244.4770          2
#> 4  LINESTRING (1200 -1581, 127...            3    210.3450          2
#> 5  LINESTRING (1174 -1684, 153...            4    375.4797          3
#> 6  LINESTRING (1223 -1898, 126...            5    145.4648          1
#> 7  LINESTRING (946 -1386, 1032...            6    596.2349          2
#> 8  LINESTRING (1350 -1896, 137...            7    205.1828          2
#> 9  LINESTRING (1117 -1979, 112...            8    385.0117          2
#> 10 LINESTRING (988 -1876, 1261...            9    273.1483          1

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
oneToAllTraverse(
  shapeGraph,
  traversalType = TraversalType$Topological,
  fromX = 1217.1,
  fromY = -1977.3
)
#> Simple feature collection with 173 features and 11 fields
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
#>    Axial Line Ref Data Map Ref Segment Length Topological Step Depth
#> 1              -1            0      65.799698                      0
#> 2              -1            1      77.203362                      0
#> 3              -1            2      90.154808                      0
#> 4              -1            3      91.088219                      0
#> 5              -1            4      35.005592                      0
#> 6              -1            5      69.541687                      0
#> 7              -1            6      37.176987                      0
#> 8              -1            7      35.278736                      0
#> 9              -1            8      34.014023                      0
#> 10             -1            9       9.775865                      0
```
