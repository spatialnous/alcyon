# Create a LatticeMap through a grid

Create a LatticeMap through a grid

## Usage

``` r
createGrid(minX, minY, maxX, maxY, gridSize, verbose = FALSE)
```

## Arguments

- minX:

  Minimum X of the bounding region

- minY:

  Minimum Y of the bounding region

- maxX:

  Maximum X of the bounding region

- maxY:

  Maximum Y of the bounding region

- gridSize:

  Size of the cells

- verbose:

  Optional. Show more information of the process.

## Value

A new LatticeMap

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
createGrid(
  minX = mapRegion[["xmin"]],
  minY = mapRegion[["ymin"]],
  maxX = mapRegion[["xmax"]],
  maxY = mapRegion[["ymax"]],
  gridSize = 0.04
)
#> stars object with 2 dimensions and 1 attribute
#> attribute(s):
#>      Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#> Ref     0 2424933 4915267 4915267 7405601 9830534
#> dimension(s):
#>   from  to offset delta x/y
#> x    1 151   1.86  0.04 [x]
#> y    1 135   6.94 -0.04 [y]
```
