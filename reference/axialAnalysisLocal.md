# Axial analysis - local metrics

Runs axial analysis to get the local metrics Control and Controllability

## Usage

``` r
axialAnalysisLocal(shapeGraph, copyMap = TRUE, verbose = FALSE)
```

## Arguments

- shapeGraph:

  An Axial ShapeGraph

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
mifFile <- system.file(
    "extdata", "testdata", "barnsbury",
    "barnsbury_small_axial_original.mif",
    package = "alcyon"
  )
  sfMap <- st_read(mifFile,
    geometry_column = 1L, quiet = TRUE
  )
  shapeGraph <- as(sfMap, "AxialShapeGraph")
axialAnalysisLocal(shapeGraph)
#> Simple feature collection with 58 features and 8 fields
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
#>                          geometry Data Map Ref Line Length  Control
#> 1  LINESTRING (984 -1684, 1346...            0    396.1881 1.827381
#> 2  LINESTRING (1306 -1497, 137...            1    259.4803 1.642857
#> 3  LINESTRING (1257 -1772, 149...            2    244.4770 1.726190
#> 4  LINESTRING (1200 -1581, 127...            3    210.3450 1.851190
#> 5  LINESTRING (1174 -1684, 153...            4    375.4797 1.101190
#> 6  LINESTRING (1223 -1898, 126...            5    145.4648 2.177381
#> 7  LINESTRING (946 -1386, 1032...            6    596.2349 1.327778
#> 8  LINESTRING (1350 -1896, 137...            7    205.1828 1.235714
#> 9  LINESTRING (1117 -1979, 112...            8    385.0117 1.661111
#> 10 LINESTRING (988 -1876, 1261...            9    273.1483 2.494048
#>    Controllability
#> 1        0.2727273
#> 2        0.2800000
#> 3        0.2916667
#> 4        0.2758621
#> 5        0.3333333
#> 6        0.2285714
#> 7        0.2222222
#> 8        0.1935484
#> 9        0.2413793
#> 10       0.3125000
```
