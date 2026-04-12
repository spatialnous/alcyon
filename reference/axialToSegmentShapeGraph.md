# Axial to Segment ShapeGraph

Convert an Axial ShapeGraph to a Segment ShapeGraph

## Usage

``` r
axialToSegmentShapeGraph(axialShapeGraph, stubRemoval = NULL)
```

## Arguments

- axialShapeGraph:

  An Axial ShapeGraph

- stubRemoval:

  Remove stubs of axial lines shorter than this percentage (for example
  provide 0.4 for 40%)

## Value

A new Segment ShapeGraph

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
axialToSegmentShapeGraph(shapeGraph, stubRemoval = 0.4)
#> Simple feature collection with 191 features and 13 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 946 ymin: -2177 xmax: 1537 ymax: -1386
#> CRS:           NA
#> First 10 features:
#>    Angular Connectivity Axial Connectivity Axial Data Map Ref Axial Line Length
#> 1              3.155704                  6                  0          396.1881
#> 2              2.115669                  6                  0          396.1881
#> 3              1.775849                  6                  0          396.1881
#> 4              2.034886                  6                  0          396.1881
#> 5              2.002752                  6                  0          396.1881
#> 6              2.052529                  7                  1          259.4803
#> 7              3.032331                  7                  1          259.4803
#> 8              3.051960                  7                  1          259.4803
#> 9              2.948040                  7                  1          259.4803
#> 10             4.000000                  7                  1          259.4803
#>    Axial Line Ref Axial df_1_Depthmap_Ref Axial df_2_Connectivity
#> 1               0                       0                       6
#> 2               0                       0                       6
#> 3               0                       0                       6
#> 4               0                       0                       6
#> 5               0                       0                       6
#> 6               1                       1                       7
#> 7               1                       1                       7
#> 8               1                       1                       7
#> 9               1                       1                       7
#> 10              1                       1                       7
#>    Axial df_3_Data_Map_Ref Axial df_4_Line_Length Axial df_row_name
#> 1                        0               396.1881                 1
#> 2                        0               396.1881                 1
#> 3                        0               396.1881                 1
#> 4                        0               396.1881                 1
#> 5                        0               396.1881                 1
#> 6                        1               259.4803                 2
#> 7                        1               259.4803                 2
#> 8                        1               259.4803                 2
#> 9                        1               259.4803                 2
#> 10                       1               259.4803                 2
#>    Connectivity Ref Segment Length                       geometry
#> 1             4   0      65.799698 LINESTRING (989.1063 -1681....
#> 2             4   1      77.203362 LINESTRING (1049.228 -1654....
#> 3             4   2      90.154808 LINESTRING (1119.769 -1623....
#> 4             4   3      91.088219 LINESTRING (1202.144 -1586....
#> 5             3   4      35.005592 LINESTRING (1285.372 -1549....
#> 6             3   5      69.541687 LINESTRING (1317.357 -1535....
#> 7             5   6      37.176987 LINESTRING (1336.921 -1602....
#> 8             5   7      35.278736 LINESTRING (1347.381 -1638....
#> 9             5   8      34.014023 LINESTRING (1357.306 -1672....
#> 10            6   9       9.775865 LINESTRING (1366.875 -1704....
```
