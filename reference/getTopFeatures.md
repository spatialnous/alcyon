# Extract top x percent of features

Sorts features by a specific column and extracts the top x percent

## Usage

``` r
getTopFeatures(lineStringMap, column, percent)
```

## Arguments

- lineStringMap:

  An sf lineString map

- column:

  The column to use to extract the features from

- percent:

  Percentage of features (to total) to extract

## Value

The lineString map filtered and sorted

## Examples

``` r
mifFile <- system.file(
    "extdata", "testdata", "barnsbury",
    "barnsbury_small_axial_original.mif",
    package = "alcyon"
  )
  sfMap <- st_read(mifFile,
    geometry_column = 1L, quiet = TRUE
  )
  shapeGraph <- as(sfMap, "AxialShapeGraph")
shapeGraph <- allToAllTraverse(
  shapeGraph,
  traversalType = TraversalType$Topological,
  radii = c("n", "3"),
  includeBetweenness = TRUE
)
getTopFeatures(shapeGraph, "Connectivity", 0.1)
#> Simple feature collection with 6 features and 28 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 973 ymin: -2058 xmax: 1359 ymax: -1581
#> CRS:           NA
#>    Depthmap_Ref Connectivity Data_Map_Ref Line_Length
#> 17           16            7           16    213.8832
#> 4             3            8            3    210.3450
#> 6             5            8            5    145.4648
#> 20           19            9           19    226.0022
#> 10            9           10            9    273.1483
#> 13           12           12           12    193.4942
#>                          geometry Data Map Ref Line Length Choice Choice R3
#> 17 LINESTRING (1236 -1847, 127...           16    213.8831    274       107
#> 4  LINESTRING (1200 -1581, 127...            3    210.3450    454       161
#> 6  LINESTRING (1223 -1898, 126...            5    145.4648    511       220
#> 20 LINESTRING (973 -1752, 1199...           19    226.0022    472       184
#> 10 LINESTRING (988 -1876, 1261...            9    273.1483    654       255
#> 13 LINESTRING (1167 -1874, 135...           12    193.4942    659       326
#>    Choice [Norm] Choice [Norm] R3  Entropy Entropy R3 Harmonic Mean Depth
#> 17     0.1716792        0.1080808 1.973692   1.460506            5.223067
#> 4      0.2844611        0.1214178 1.758874   1.463609           14.419703
#> 6      0.3201754        0.1659125 1.731490   1.433642           14.256503
#> 20     0.2957394        0.2358974 2.044582   1.536169            5.368661
#> 10     0.4097744        0.1923077 1.864475   1.511067            4.842767
#> 13     0.4129073        0.3015726 1.941796   1.550662           20.000000
#>    Harmonic Mean Depth R3 Integration [HH] Integration [HH] R3
#> 17               16.04494         2.105129            2.527273
#> 4                18.51018         2.361852            2.543419
#> 6                18.11097         2.548314            2.793591
#> 20               16.57253         1.936718            2.447779
#> 10               20.46512         2.482972            2.748533
#> 13               20.00000         2.420898            2.956538
#>    Integration [P-value] Integration [P-value] R3 Integration [Tekl]
#> 17              2.105129                 2.527273          0.7369218
#> 4               2.361852                 2.543419          0.7561645
#> 6               2.548314                 2.793591          0.7694319
#> 20              1.936718                 2.447779          0.7235790
#> 10              2.482972                 2.748533          0.7648444
#> 13              2.420898                 2.956538          0.7604254
#>    Integration [Tekl] R3 Intensity Intensity R3 Mean Depth Mean Depth R3
#> 17             0.7713465  1.257958     1.244135   2.614035      2.222222
#> 4              0.7702533  1.259441     1.175322   2.438596      2.288461
#> 6              0.7878321  1.339019     1.266384   2.333333      2.173077
#> 20             0.7673109  1.197836     1.340062   2.754386      2.200000
#> 10             0.7847281  1.404410     1.312894   2.368421      2.192308
#> 13             0.8015020  1.425622     1.519016   2.403509      2.063830
#>    Node Count Node Count R3 Relativised Entropy Relativised Entropy R3
#> 17         58            46            1.641994               1.710227
#> 4          58            53            1.662805               1.783778
#> 6          58            53            1.593437               1.692109
#> 20         58            41            1.723361               1.641913
#> 10         58            53            1.526621               1.657888
#> 13         58            48            1.524789               1.500373
```
