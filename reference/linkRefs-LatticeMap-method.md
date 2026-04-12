# Link two LatticeMap Cells (refs)

Link two cells on an LatticeMap using their refs

## Usage

``` r
# S4 method for class 'LatticeMap'
linkRefs(map, fromRef, toRef, copyMap = TRUE)
```

## Arguments

- map:

  A LatticeMap

- fromRef:

  Ref of the first link line

- toRef:

  Ref of the second link line

- copyMap:

  Optional. Copy the internal sala map

## Value

A new LatticeMap with linked points

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
latticeMap <- linkRefs(latticeMap, 1835056L, 7208971L)
```
