# Segment Tulip Leaf Choice

This is the legacy calculation of choice where backwards traversal was
only started from "leaf" nodes i.e. nodes where the forwards traversal
originally found a dead end (from actual dead ends, to dead ends because
the next line has been covered through a different path)

## Usage

``` r
segmentTulipLeafChoice(
  map,
  radii,
  radiusTraversalType,
  weightByAttribute = NULL,
  quantizationWidth = NA,
  copyMap = TRUE,
  verbose = FALSE,
  progress = FALSE
)
```

## Arguments

- map:

  A Segment ShapeGraph

- radii:

  A list of radii

- radiusTraversalType:

  The traversal type to keep track of whether the analysis is within the
  each radius limit. See
  [TraversalType](https://spatialnous.github.io/alcyon/reference/TraversalType.md)

- weightByAttribute:

  The attribute to weigh the analysis with

- quantizationWidth:

  Set this to use chunks of this width instead of continuous values for
  the cost of traversal. This is equivalent to the "tulip bins" for
  depthmapX's tulip analysis (1024 tulip bins = pi/1024
  quantizationWidth). Only works for Segment ShapeGraphs

- copyMap:

  Optional. Copy the internal sala map

- verbose:

  Optional. Show more information of the process.

- progress:

  Optional. Enable progress display

## Value

A new map with the results included

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
allToAllTraverse(latticeMap,
  traversalType = TraversalType$Angular,
  radii = -1L,
  radiusTraversalType = TraversalType$None
)
#> stars object with 2 dimensions and 10 attributes
#> attribute(s):
#> Warning: number of columns of result is not a multiple of vector length (arg 1)
#>                              Min.      1st Qu.       Median         Mean
#> Ref                  0.000000e+00 1.966108e+05 3.932215e+05 3.932215e+05
#> Connectivity         3.900000e+01 6.500000e+01 7.500000e+01 7.106667e+01
#> Point First Moment   9.203807e+01 1.451355e+02 1.732670e+02 1.750389e+02
#> Point Second Moment  2.400000e+02 3.885625e+02 5.315000e+02 5.387111e+02
#> blocked              0.000000e+00 0.000000e+00 0.000000e+00 3.205128e-01
#> contextfilled        0.000000e+00 0.000000e+00 0.000000e+00 0.000000e+00
#> filled               0.000000e+00 0.000000e+00 1.000000e+00 5.769231e-01
#> Angular Mean Depth   1.546766e-03 2.146721e-02 6.614886e-02 1.193368e-01
#> Angular Node Count   9.000000e+01 9.000000e+01 9.000000e+01 9.000000e+01
#> Angular Total Depth  1.392090e-01 1.932049e+00 5.953397e+00 1.074031e+01
#>                           3rd Qu.         Max. NAs NA's
#> Ref                  5.898322e+05 7.864430e+05   0    0
#> Connectivity         8.100000e+01 8.800000e+01  66    0
#> Point First Moment   2.021131e+02 2.921885e+02  66    0
#> Point Second Moment  6.655625e+02 1.157750e+03  66    0
#> blocked              1.000000e+00 1.000000e+00   0    0
#> contextfilled        0.000000e+00 0.000000e+00   0    0
#> filled               1.000000e+00 1.000000e+00   0    0
#> Angular Mean Depth   1.826283e-01 4.806018e-01  66    0
#> Angular Node Count   9.000000e+01 9.000000e+01  66    0
#> Angular Total Depth  1.643655e+01 4.325416e+01  66    0
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
allToAllTraverse(
  shapeGraph,
  traversalType = TraversalType$Topological,
  radii = c("n", "3"),
  includeBetweenness = TRUE
)
#> Simple feature collection with 58 features and 28 fields
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
#>                          geometry Data Map Ref Line Length Choice Choice R3
#> 1  LINESTRING (984 -1684, 1346...            0    396.1881    219        82
#> 2  LINESTRING (1306 -1497, 137...            1    259.4803    287       108
#> 3  LINESTRING (1257 -1772, 149...            2    244.4770    340       130
#> 4  LINESTRING (1200 -1581, 127...            3    210.3450    475       153
#> 5  LINESTRING (1174 -1684, 153...            4    375.4797    174        35
#> 6  LINESTRING (1223 -1898, 126...            5    145.4648    487       210
#> 7  LINESTRING (946 -1386, 1032...            6    596.2349    295       109
#> 8  LINESTRING (1350 -1896, 137...            7    205.1828    557       165
#> 9  LINESTRING (1117 -1979, 112...            8    385.0117    320       130
#> 10 LINESTRING (988 -1876, 1261...            9    273.1483    702       263
#>    Choice [Norm] Choice [Norm] R3  Entropy Entropy R3 Harmonic Mean Depth
#> 1      0.1372180       0.09080841 1.966392   1.431980            5.170979
#> 2      0.1798246       0.09183674 1.813029   1.435994           16.434782
#> 3      0.2130326       0.12560387 1.872506   1.452452           17.749153
#> 4      0.2976190       0.11538462 1.758874   1.463609           14.419703
#> 5      0.1090226       0.07526882 1.937888   1.321543            8.701672
#> 6      0.3051378       0.15837105 1.731490   1.433642           14.256503
#> 7      0.1848371       0.10531401 1.934941   1.426639            5.121012
#> 8      0.3489975       0.11973875 1.658470   1.394359           11.949306
#> 9      0.2005012       0.12560387 1.955858   1.452452            5.196212
#> 10     0.4398496       0.19834088 1.864475   1.511067            4.842767
#>    Harmonic Mean Depth R3 Integration [HH] Integration [HH] R3
#> 1               14.451612         1.917543            2.246524
#> 2               16.661158         2.151909            2.375361
#> 3               16.185471         2.060338            2.350789
#> 4               18.510180         2.361852            2.543419
#> 5                9.353847         1.524975            1.762609
#> 6               18.110973         2.548314            2.793591
#> 7               14.985915         2.082493            2.430477
#> 8               15.865385         2.391010            2.536365
#> 9               16.185471         2.151909            2.560681
#> 10              20.465117         2.482972            2.748533
#>    Integration [P-value] Integration [P-value] R3 Integration [Tekl]
#> 1               1.917543                 2.246524          0.7220190
#> 2               2.151909                 2.375361          0.7405212
#> 3               2.060338                 2.350789          0.7334335
#> 4               2.361852                 2.543419          0.7561645
#> 5               1.524975                 1.762609          0.6878769
#> 6               2.548314                 2.793591          0.7694319
#> 7               2.082493                 2.430477          0.7351641
#> 8               2.391010                 2.536365          0.7582758
#> 9               2.151909                 2.560681          0.7405212
#> 10              2.482972                 2.748533          0.7648444
#>    Integration [Tekl] R3 Intensity Intensity R3 Mean Depth Mean Depth R3
#> 1              0.7498006 1.1405076     1.105388   2.771930      2.348837
#> 2              0.7585479 1.1815245     1.104611   2.578947      2.346939
#> 3              0.7573853 1.1677991     1.137754   2.649123      2.326087
#> 4              0.7702533 1.2594405     1.175322   2.438596      2.288461
#> 5              0.7073136 0.8920435     0.939764   3.228070      2.483871
#> 6              0.7878321 1.3390193     1.266384   2.333333      2.173077
#> 7              0.7635773 1.2198540     1.156069   2.631579      2.282609
#> 8              0.7694886 1.2023906     1.107285   2.421053      2.301887
#> 9              0.7734766 1.2746043     1.241186   2.578947      2.217391
#> 10             0.7847281 1.4044101     1.312894   2.368421      2.192308
#>    Node Count Node Count R3 Relativised Entropy Relativised Entropy R3
#> 1          58            44            1.760874               1.853100
#> 2          58            50            1.734981               1.858356
#> 3          58            47            1.748742               1.822981
#> 4          58            53            1.662805               1.783778
#> 5          58            32            2.097106               2.047915
#> 6          58            53            1.593437               1.692109
#> 7          58            47            1.676330               1.793509
#> 8          58            54            1.720402               1.844227
#> 9          58            47            1.626325               1.712716
#> 10         58            53            1.526621               1.657888

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
segmentTulipLeafChoice(
  shapeGraph,
  radii = c("n", "100"),
  radiusTraversalType = TraversalType$Metric,
  weightByAttribute = "Segment Length",
  quantizationWidth = pi / 1024L,
  verbose = FALSE,
  progress = FALSE
)
#> Simple feature collection with 173 features and 16 fields
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
#>    Axial Line Ref Data Map Ref Segment Length T1024 Leaf T1024 Leaf Choice
#> 1              -1            0      65.799698          0               203
#> 2              -1            1      77.203362          1               484
#> 3              -1            2      90.154808          0              1406
#> 4              -1            3      91.088219          2               674
#> 5              -1            4      35.005592          0               497
#> 6              -1            5      69.541687          0               526
#> 7              -1            6      37.176987          0               767
#> 8              -1            7      35.278736          0              1348
#> 9              -1            8      34.014023          1              2059
#> 10             -1            9       9.775865          0              3136
#>    T1024 Leaf Choice R100.00 metric T1024 Leaf Choice [Segment Length Wgt]
#> 1                                 2                               659726.2
#> 2                                 0                               997757.4
#> 3                                 0                              2753404.2
#> 4                                 0                              1370659.9
#> 5                                 2                               910519.9
#> 6                                 0                               974137.1
#> 7                                 2                              1566921.5
#> 8                                 7                              2342901.8
#> 9                                18                              3151169.5
#> 10                               19                              3774213.8
#>    T1024 Leaf Choice [Segment Length Wgt] R100.00 metric
#> 1                                               23681.04
#> 2                                               16837.18
#> 3                                               23477.67
#> 4                                               20490.54
#> 5                                               12713.72
#> 6                                               18764.73
#> 7                                               19753.70
#> 8                                               19874.78
#> 9                                               25365.45
#> 10                                              32525.98
#>    T1024 Leaf R100.00 metric
#> 1                          3
#> 2                          3
#> 3                          3
#> 4                          3
#> 5                          3
#> 6                          3
#> 7                          3
#> 8                          3
#> 9                          3
#> 10                         3
```
