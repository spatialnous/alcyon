# Create an All-line Map

Create an All-line Map

## Usage

``` r
makeAllLineMap(boundsMap, seedX, seedY, verbose = FALSE)
```

## Arguments

- boundsMap:

  The boundary ShapeMap to create the all-line map in

- seedX:

  X coordinate of the seed (the point that initiates the process)

- seedY:

  Y coordinate of the seed (the point that initiates the process)

- verbose:

  Optional. Show more information of the process.

## Value

An All-line Axial ShapeGraph

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
makeAllLineMap(
  shapeMap,
  seedX = 3.01,
  seedY = 6.7
)
#> Simple feature collection with 7 features and 3 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 1.888668 ymin: 1.544019 xmax: 7.89713 ymax: 6.908548
#> CRS:           NA
#>   Connectivity Line Length Ref                       geometry
#> 1            6    6.046897   0 LINESTRING (1.888668 6.9085...
#> 2            6    6.810349   1 LINESTRING (1.888668 6.9085...
#> 3            5    6.267958   2 LINESTRING (1.888668 6.9085...
#> 4            6    6.117036   3 LINESTRING (3.144843 6.9085...
#> 5            6    6.906496   4 LINESTRING (1.888668 1.5609...
#> 6            5    6.918201   5 LINESTRING (1.888668 3.4537...
#> 7            4    4.764533   6 LINESTRING (1.888668 1.5609...
```
