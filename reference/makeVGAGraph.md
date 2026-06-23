# Create a graph between visible cells in the LatticeMap

Create a graph between visible cells in the LatticeMap

## Usage

``` r
makeVGAGraph(
  latticeMap,
  boundaryGraph = FALSE,
  maxVisibility = NA,
  copyMap = TRUE,
  verbose = FALSE
)
```

## Arguments

- latticeMap:

  The input LatticeMap

- boundaryGraph:

  Only create a graph on the boundary cells

- maxVisibility:

  Limit how far two cells can be to be connected

- copyMap:

  Optional. Copy the internal sala map

- verbose:

  Optional. Show more information of the process.

## Value

A new LatticeMap with a graph between points

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
  gridSize = 0.5
)
latticeMap <- blockLines(
  latticeMap = latticeMap,
  lineStringMap = lineStringMap[vector()]
)
latticeMap <- fillGrid(
  latticeMap = latticeMap,
  fillX = 3.01,
  fillY = 6.7
)
makeVGAGraph(
  latticeMap = latticeMap,
  boundaryGraph = FALSE,
  maxVisibility = NA
)
#> stars object with 2 dimensions and 7 attributes
#> attribute(s):
#> Warning: number of columns of result is not a multiple of vector length (arg 1)
#>                           Min.     1st Qu.     Median         Mean     3rd Qu.
#> Ref                    0.00000 196610.7500 393221.500 3.932215e+05 589832.2500
#> blocked                0.00000      0.0000      0.000 3.205128e-01      1.0000
#> contextfilled          0.00000      0.0000      0.000 0.000000e+00      0.0000
#> filled                 0.00000      0.0000      1.000 5.769231e-01      1.0000
#> Connectivity          39.00000     65.0000     75.000 7.106667e+01     81.0000
#> Point First Moment    92.03807    145.1355    173.267 1.750389e+02    202.1131
#> Point Second Moment  240.00000    388.5625    531.500 5.387111e+02    665.5625
#>                             Max. NAs NA's
#> Ref                  786443.0000   0    0
#> blocked                   1.0000   0    0
#> contextfilled             0.0000   0    0
#> filled                    1.0000   0    0
#> Connectivity             88.0000  66    0
#> Point First Moment      292.1885  66    0
#> Point Second Moment    1157.7500  66    0
#> dimension(s):
#>   from to offset delta x/y
#> x    1 13   1.75   0.5 [x]
#> y    1 12   7.25  -0.5 [y]
```
