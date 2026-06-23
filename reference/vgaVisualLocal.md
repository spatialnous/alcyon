# Visibility Graph Analysis - Visual local metrics

Runs Visibility Graph Analysis to get visual local metrics

## Usage

``` r
vgaVisualLocal(
  latticeMap,
  nthreads = 1L,
  algorithm = VGALocalAlgorithm$Standard,
  copyMap = TRUE,
  gatesOnly = FALSE,
  progress = FALSE
)
```

## Arguments

- latticeMap:

  A LatticeMap

- nthreads:

  Optional. Number of threads to use (defaults to 1)

- algorithm:

  Optional. The algorithm to use. See ?VGALocalAlgorithm

- copyMap:

  Optional. Copy the internal sala map

- gatesOnly:

  Optional. Only keep the values at specific gates

- progress:

  Optional. Enable progress display

## Value

A new LatticeMap with the results included

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
  latticeMap <- makeVGALatticeMap(
    sfMap,
    gridSize = 0.5,
    fillX = 3.0,
    fillY = 6.0,
    maxVisibility = NA,
    boundaryGraph = FALSE,
    verbose = FALSE
  )
vgaVisualLocal(latticeMap, FALSE)
#> stars object with 2 dimensions and 10 attributes
#> attribute(s):
#> Warning: number of columns of result is not a multiple of vector length (arg 1)
#>                                       Min.      1st Qu.       Median
#> Ref                              0.0000000 1.966108e+05 3.932215e+05
#> Connectivity                    39.0000000 6.500000e+01 7.500000e+01
#> Point First Moment              92.0380707 1.451355e+02 1.732670e+02
#> Point Second Moment            240.0000000 3.885625e+02 5.315000e+02
#> blocked                          0.0000000 0.000000e+00 0.000000e+00
#> contextfilled                    0.0000000 0.000000e+00 0.000000e+00
#> filled                           0.0000000 0.000000e+00 1.000000e+00
#> Visual Clustering Coefficient    0.8014629 8.408053e-01 9.060150e-01
#> Visual Control                   0.5896810 8.775250e-01 1.012651e+00
#> Visual Controllability           0.4382023 7.222222e-01 8.333333e-01
#>                                        Mean      3rd Qu.         Max. NAs NA's
#> Ref                            3.932215e+05 5.898322e+05 7.864430e+05   0    0
#> Connectivity                   7.106667e+01 8.100000e+01 8.800000e+01  66    0
#> Point First Moment             1.750389e+02 2.021131e+02 2.921885e+02  66    0
#> Point Second Moment            5.387111e+02 6.655625e+02 1.157750e+03  66    0
#> blocked                        3.205128e-01 1.000000e+00 1.000000e+00   0    0
#> contextfilled                  0.000000e+00 0.000000e+00 0.000000e+00   0    0
#> filled                         5.769231e-01 1.000000e+00 1.000000e+00   0    0
#> Visual Clustering Coefficient  9.014550e-01 9.466245e-01 1.000000e+00  66    0
#> Visual Control                 1.000000e+00 1.169747e+00 1.295770e+00  66    0
#> Visual Controllability         7.899721e-01 9.000000e-01 9.777778e-01  66    0
#> dimension(s):
#>   from to offset delta x/y
#> x    1 13   1.75   0.5 [x]
#> y    1 12   7.25  -0.5 [y]
```
