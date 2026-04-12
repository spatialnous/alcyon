# Unlink two LatticeMap Cells (coordinates)

Unlink two cells on a LatticeMap using the point coordinates

## Usage

``` r
# S4 method for class 'LatticeMap'
unlinkCoords(map, fromX, fromY, toX, toY, copyMap = TRUE)
```

## Arguments

- map:

  A LatticeMap

- fromX:

  X coordinate of the first unlink point

- fromY:

  Y coordinate of the first unlink point

- toX:

  X coordinate of the second unlink point

- toY:

  Y coordinate of the second unlink point

- copyMap:

  Optional. Copy the internal sala map

## Value

A new LatticeMap with unlinked points

## Examples

``` r
mifFile <- system.file(
    "extdata", "testdata", "gallery",
    "gallery_lines.mif",
    package = "alcyon"
  )
  sfMap <- st_read(mifFile,
    geometry_column = 1L, quiet = TRUE
  )
  latticeMap <- makeVGALatticeMap(
    sfMap,
    gridSize = 0.04,
    fillX = 3.01,
    fillY = 6.7,
    maxVisibility = NA,
    boundaryGraph = FALSE,
    verbose = FALSE
  )
latticeMap <- linkCoords(latticeMap, 1.74, 6.7, 5.05, 5.24)
latticeMap <- unlinkCoords(latticeMap, 1.74, 6.7, 5.05, 5.24)
```
