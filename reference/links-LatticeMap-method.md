# Get the LatticeMap links

Get the LatticeMap links

## Usage

``` r
# S4 method for class 'LatticeMap'
links(map)
```

## Arguments

- map:

  A LatticeMap

## Value

A matrix with the linked refs

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
linkRefs(latticeMap, 1835056L, 7208971L)
#> stars object with 2 dimensions and 7 attributes
#> attribute(s):
#> Warning: number of columns of result is not a multiple of vector length (arg 1)
#>                           Min.      1st Qu.       Median         Mean
#> Ref                  0.0000000 1835064.7500 3.735590e+06 3.735590e+06
#> Connectivity         9.0000000     107.0000 1.790000e+02 2.048587e+02
#> Point First Moment   0.6780338      30.5024 7.556305e+01 1.097780e+02
#> Point Second Moment  0.0560000      12.0508 4.336080e+01 1.115533e+02
#> blocked              0.0000000       0.0000 0.000000e+00 1.989703e-01
#> contextfilled        0.0000000       0.0000 0.000000e+00 0.000000e+00
#> filled               0.0000000       0.0000 0.000000e+00 4.956522e-01
#>                           3rd Qu.         Max.  NAs NA's
#> Ref                  5636114.2500 7471179.0000    0    0
#> Connectivity             284.0000     624.0000 4408    0
#> Point First Moment       149.7499     619.3036 4408    0
#> Point Second Moment      105.2376    1218.8016 4408    0
#> blocked                    0.0000       1.0000    0    0
#> contextfilled              0.0000       0.0000    0    0
#> filled                     1.0000       1.0000    0    0
#> dimension(s):
#>   from  to offset delta x/y
#> x    1 115   0.62  0.04 [x]
#> y    1  76   7.82 -0.04 [y]
links(latticeMap)
#>      from to
```
