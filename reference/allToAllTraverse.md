# All-to-all traversal

Runs all-to-all traversal on a map with a graph. This is applicable to:

- LatticeMaps (Visibility Graph Analysis)

- Axial ShapeGraphs (Axial analysis)

- Segment ShapeGraphs (Segment analysis)

## Usage

``` r
allToAllTraverse(
  map,
  traversalType,
  radii,
  radiusTraversalType,
  weightByAttribute = NULL,
  includeBetweenness = FALSE,
  quantizationWidth = NA,
  gatesOnly = FALSE,
  nthreads = 1L,
  copyMap = TRUE,
  verbose = FALSE,
  progress = FALSE
)
```

## Arguments

- map:

  A LatticeMap, Axial ShapeGraph or Segment ShapeGraph

- traversalType:

  The traversal type. See
  [TraversalType](https://spatialnous.github.io/alcyon/reference/TraversalType.md)

- radii:

  A list of radii

- radiusTraversalType:

  The traversal type to keep track of whether the analysis is within the
  each radius limit. See
  [TraversalType](https://spatialnous.github.io/alcyon/reference/TraversalType.md)

- weightByAttribute:

  The attribute to weigh the analysis with

- includeBetweenness:

  Set to TRUE to also calculate betweenness (known as Choice in the
  Space Syntax domain)

- quantizationWidth:

  Set this to use chunks of this width instead of continuous values for
  the cost of traversal. This is equivalent to the "tulip bins" for
  depthmapX's tulip analysis (1024 tulip bins = pi/1024
  quantizationWidth). Only works for Segment ShapeGraphs

- gatesOnly:

  Optional. Only calculate results at particular gate pixels. Only works
  for LatticeMaps

- nthreads:

  Optional. Use more than one threads. 1 by default, set to 0 to use all
  available. Only available for LatticeMaps.

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
#> 1  LINESTRING (984 -1684, 1346...            0    396.1881    205        86
#> 2  LINESTRING (1306 -1497, 137...            1    259.4803    279        82
#> 3  LINESTRING (1257 -1772, 149...            2    244.4770    328       128
#> 4  LINESTRING (1200 -1581, 127...            3    210.3450    493       191
#> 5  LINESTRING (1174 -1684, 153...            4    375.4797    118        24
#> 6  LINESTRING (1223 -1898, 126...            5    145.4648    502       216
#> 7  LINESTRING (946 -1386, 1032...            6    596.2349    202        78
#> 8  LINESTRING (1350 -1896, 137...            7    205.1828    591       160
#> 9  LINESTRING (1117 -1979, 112...            8    385.0117    342       144
#> 10 LINESTRING (988 -1876, 1261...            9    273.1483    647       248
#>    Choice [Norm] Choice [Norm] R3  Entropy Entropy R3 Harmonic Mean Depth
#> 1     0.12844612       0.09523810 1.966392   1.431980            5.170979
#> 2     0.17481203       0.06972789 1.813029   1.435994           16.434782
#> 3     0.20551379       0.12367149 1.872506   1.452452           17.749153
#> 4     0.30889726       0.14404224 1.758874   1.463609           14.419703
#> 5     0.07393484       0.05161290 1.937888   1.321543            8.701672
#> 6     0.31453633       0.16289593 1.731490   1.433642           14.256503
#> 7     0.12656641       0.07536232 1.934941   1.426639            5.121012
#> 8     0.37030074       0.11611030 1.658470   1.394359           11.949306
#> 9     0.21428572       0.13913043 1.955858   1.452452            5.196212
#> 10    0.40538847       0.18702866 1.864475   1.511067            4.842767
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
allToAllTraverse(
  shapeGraph,
  radii = c("n", "100"),
  radiusTraversalType = TraversalType$Metric,
  traversalType = TraversalType$Angular,
  weightByAttribute = "Segment Length",
  includeBetweenness = TRUE,
  quantizationWidth = pi / 1024L,
  verbose = FALSE,
  progress = FALSE
)
#> Simple feature collection with 173 features and 26 fields
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
#>    Axial Line Ref Data Map Ref Segment Length T1024 Choice
#> 1              -1            0      65.799698          344
#> 2              -1            1      77.203362         1090
#> 3              -1            2      90.154808         2358
#> 4              -1            3      91.088219         1572
#> 5              -1            4      35.005592         1219
#> 6              -1            5      69.541687         1244
#> 7              -1            6      37.176987         1685
#> 8              -1            7      35.278736         2437
#> 9              -1            8      34.014023         3691
#> 10             -1            9       9.775865         5252
#>    T1024 Choice R100.00 metric T1024 Choice [Segment Length Wgt]
#> 1                            2                           1143457
#> 2                            0                           1943518
#> 3                            0                           4310124
#> 4                            0                           2743090
#> 5                            2                           2056683
#> 6                            0                           2144044
#> 7                            2                           3170765
#> 8                            7                           4191692
#> 9                           21                           5487972
#> 10                          28                           6455423
#>    T1024 Choice [Segment Length Wgt] R100.00 metric T1024 Integration
#> 1                                          24575.76          87.44378
#> 2                                          19081.64          88.57600
#> 3                                          23477.67          90.19110
#> 4                                          20490.54          91.37754
#> 5                                          12713.72          91.42333
#> 6                                          18764.73         106.23274
#> 7                                          19753.70         106.20918
#> 8                                          20667.12         106.32710
#> 9                                          27880.42         106.71213
#> 10                                         36701.89         107.06703
#>    T1024 Integration R100.00 metric T1024 Integration [Segment Length Wgt]
#> 1                          9.230036                               3224.680
#> 2                         11.808656                               3262.657
#> 3                         12.764543                               3379.409
#> 4                         12.047059                               3426.279
#> 5                          7.984406                               3428.560
#> 6                          8.870068                               3696.910
#> 7                         15.463087                               3693.893
#> 8                         23.844761                               3697.772
#> 9                         23.322910                               3714.063
#> 10                        21.867201                               3726.309
#>    T1024 Integration [Segment Length Wgt] R100.00 metric T1024 Node Count
#> 1                                               396.2036              173
#> 2                                              1144.3008              173
#> 3                                              1431.5309              173
#> 4                                               995.1865              173
#> 5                                               494.8211              173
#> 6                                               485.0021              173
#> 7                                               711.6516              173
#> 8                                               912.5288              173
#> 9                                               836.5922              173
#> 10                                              819.1635              173
#>    T1024 Node Count R100.00 metric T1024 Total Depth
#> 1                               11          342.2656
#> 2                                9          337.8906
#> 3                                6          331.8398
#> 4                                6          327.5312
#> 5                                4          327.3672
#> 6                                6          281.7305
#> 7                                9          281.7930
#> 8                               12          281.4805
#> 9                               13          280.4648
#> 10                              16          279.5352
#>    T1024 Total Depth R100.00 metric T1024 Total Depth [Segment Length Wgt]
#> 1                         13.109375                              10672.612
#> 2                          6.859375                              10548.384
#> 3                          2.820312                              10183.959
#> 4                          2.988281                              10044.646
#> 5                          2.003906                              10037.964
#> 6                          4.058594                               9309.332
#> 7                          5.238281                               9316.935
#> 8                          6.039062                               9307.161
#> 9                          7.246094                               9266.337
#> 10                        11.707031                               9235.884
#>    T1024 Total Depth [Segment Length Wgt] R100.00 metric
#> 1                                              424.32428
#> 2                                               91.94441
#> 3                                               85.85162
#> 4                                              100.36493
#> 5                                              112.90747
#> 6                                              237.47540
#> 7                                              274.22757
#> 8                                              284.32355
#> 9                                              322.21109
#> 10                                             440.65012
#>    T1024 Total Segment Length T1024 Total Segment Length R100.00 metric
#> 1                    5866.495                                  410.0229
#> 2                    5866.495                                  324.3641
#> 3                    5866.495                                  350.5699
#> 4                    5866.495                                  316.0409
#> 5                    5866.495                                  236.3662
#> 6                    5866.495                                  339.3760
#> 7                    5866.495                                  441.7629
#> 8                    5866.495                                  509.3657
#> 9                    5866.495                                  519.1910
#> 10                   5866.495                                  600.8032
```
