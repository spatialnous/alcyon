# Fill a LatticeMap's grid starting from one or more points

Fill a LatticeMap's grid starting from one or more points

## Usage

``` r
fillGrid(latticeMap, fillX, fillY, copyMap = TRUE, verbose = FALSE)
```

## Arguments

- latticeMap:

  The input LatticeMap

- fillX:

  X coordinate of the fill points

- fillY:

  Y coordinate of the fill points

- copyMap:

  Optional. Copy the internal sala map

- verbose:

  Optional. Show more information of the process.

## Value

A new LatticeMap with filled points

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
lineStringMap <- as(sfMap, "sf")
mapRegion <- sf::st_bbox(lineStringMap)
latticeMap <- createGrid(
  minX = mapRegion[["xmin"]],
  minY = mapRegion[["ymin"]],
  maxX = mapRegion[["xmax"]],
  maxY = mapRegion[["ymax"]],
  gridSize = 0.04
)
latticeMap <- blockLines(
  latticeMap = latticeMap,
  lineStringMap = lineStringMap[vector()]
)
fillGrid(
  latticeMap = latticeMap,
  fillX = 3.01,
  fillY = 6.7
)
#> stars object with 2 dimensions and 4 attributes
#> attribute(s):
#>                Min. 1st Qu.  Median         Mean 3rd Qu.    Max.
#> Ref               0 2424933 4915267 4.915267e+06 7405601 9830534
#> blocked           0       0       0 3.100319e-02       0       1
#> contextfilled     0       0       0 0.000000e+00       0       0
#> filled            0       0       1 7.347069e-01       1       1
#> dimension(s):
#>   from  to offset delta x/y
#> x    1 151   1.86  0.04 [x]
#> y    1 135   6.94 -0.04 [y]
```
