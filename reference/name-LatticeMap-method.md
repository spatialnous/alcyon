# Get the LatticeMap name

Get the LatticeMap name

## Usage

``` r
# S4 method for class 'LatticeMap'
name(map)
```

## Arguments

- map:

  A LatticeMap

## Value

The name of the LatticeMap as a string

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
name(latticeMap)
#> [1] "LatticeMap"
```
