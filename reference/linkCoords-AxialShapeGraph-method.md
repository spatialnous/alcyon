# Link two Axial Lines (coordinates)

Link two locations on an Axial ShapeGraph using the point coordinates

## Usage

``` r
# S4 method for class 'AxialShapeGraph'
linkCoords(map, fromX, fromY, toX, toY, copyMap = TRUE)
```

## Arguments

- map:

  An Axial ShapeGraph

- fromX:

  X coordinate of the first link point

- fromY:

  Y coordinate of the first link point

- toX:

  X coordinate of the second link point

- toY:

  Y coordinate of the second link point

- copyMap:

  Optional. Copy the internal sala map

## Value

A new Axial ShapeGraph with linked lines

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
linkCoords(shapeGraph, 982.8, -1620.3, 1217.1, -1977.3)
#> Simple feature collection with 58 features and 6 fields
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
#>                          geometry Data Map Ref Line Length
#> 1  LINESTRING (984 -1684, 1346...            0    396.1881
#> 2  LINESTRING (1306 -1497, 137...            1    259.4803
#> 3  LINESTRING (1257 -1772, 149...            2    244.4770
#> 4  LINESTRING (1200 -1581, 127...            3    210.3450
#> 5  LINESTRING (1174 -1684, 153...            4    375.4797
#> 6  LINESTRING (1223 -1898, 126...            5    145.4648
#> 7  LINESTRING (946 -1386, 1032...            6    596.2349
#> 8  LINESTRING (1350 -1896, 137...            7    205.1828
#> 9  LINESTRING (1117 -1979, 112...            8    385.0117
#> 10 LINESTRING (988 -1876, 1261...            9    273.1483
```
