# Block lines on a LatticeMap

Takes a LatticeMap and a ShapeMap with lines and blocks the cells on the
LatticeMap where the lines pass.

## Usage

``` r
blockLines(latticeMap, lineStringMap, copyMap = TRUE, verbose = FALSE)
```

## Arguments

- latticeMap:

  The input LatticeMap

- lineStringMap:

  Map of lines, either a ShapeMap, or an sf lineString map

- copyMap:

  Optional. Copy the internal sala map

- verbose:

  Optional. Show more information of the process.

## Value

A new LatticeMap with points as they have been blocked by the lines

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
blockLines(
  latticeMap = latticeMap,
  lineStringMap = lineStringMap[vector()]
)
#> stars object with 2 dimensions and 2 attributes
#> attribute(s):
#>          Min. 1st Qu.  Median         Mean 3rd Qu.    Max.
#> Ref         0 2424933 4915267 4.915267e+06 7405601 9830534
#> blocked     0       0       0 3.100319e-02       0       1
#> dimension(s):
#>   from  to offset delta x/y
#> x    1 151   1.86  0.04 [x]
#> y    1 135   6.94 -0.04 [y]
```
