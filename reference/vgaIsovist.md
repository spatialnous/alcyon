# Visibility Graph Analysis - isovist metrics

Runs axial analysis to get the local metrics Control and Controllability

## Usage

``` r
vgaIsovist(latticeMap, boundaryMap, copyMap = TRUE)
```

## Arguments

- latticeMap:

  A LatticeMap

- boundaryMap:

  A ShapeMap of lines

- copyMap:

  Optional. Copy the internal sala map

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
boundaryMap <- as(sfMap[, c()], "ShapeMap")
vgaIsovist(latticeMap, boundaryMap)
#> stars object with 2 dimensions and 15 attributes
#> attribute(s):
#>                                  Min.      1st Qu.       Median         Mean
#> Ref                        0.00000000 1.966108e+05 3.932215e+05 3.932215e+05
#> Connectivity              39.00000000 6.500000e+01 7.500000e+01 7.106667e+01
#> Point First Moment        92.03807068 1.451355e+02 1.732670e+02 1.750389e+02
#> Point Second Moment      240.00000000 3.885625e+02 5.315000e+02 5.387111e+02
#> blocked                    0.00000000 0.000000e+00 0.000000e+00 3.205128e-01
#> contextfilled              0.00000000 0.000000e+00 0.000000e+00 0.000000e+00
#> filled                     0.00000000 0.000000e+00 1.000000e+00 5.769231e-01
#> Isovist Area              10.97448063 1.704208e+01 1.972816e+01 1.895598e+01
#> Isovist Compactness        0.46210757 4.860164e-01 5.304640e-01 5.431781e-01
#> Isovist Drift Angle        9.82207966 1.064620e+02 1.794039e+02 1.847821e+02
#> Isovist Drift Magnitude    0.27391502 1.307139e+00 1.817429e+00 1.755722e+00
#> Isovist Max Radial         3.59407520 4.838256e+00 5.383654e+00 5.337678e+00
#> Isovist Min Radial         0.07054356 3.697004e-01 4.786798e-01 6.104857e-01
#> Isovist Occlusivity        1.45719266 2.207036e+00 3.058959e+00 3.070312e+00
#> Isovist Perimeter         14.01030445 1.875724e+01 2.116433e+01 2.098679e+01
#>                               3rd Qu.         Max. NA's
#> Ref                      5.898322e+05 7.864430e+05    0
#> Connectivity             8.100000e+01 8.800000e+01   66
#> Point First Moment       2.021131e+02 2.921885e+02   66
#> Point Second Moment      6.655625e+02 1.157750e+03   66
#> blocked                  1.000000e+00 1.000000e+00    0
#> contextfilled            0.000000e+00 0.000000e+00    0
#> filled                   1.000000e+00 1.000000e+00    0
#> Isovist Area             2.203810e+01 2.358941e+01   66
#> Isovist Compactness      5.984749e-01 7.025847e-01   66
#> Isovist Drift Angle      2.764781e+02 3.576884e+02   66
#> Isovist Drift Magnitude  2.257860e+00 3.034088e+00   66
#> Isovist Max Radial       5.894182e+00 6.769641e+00   66
#> Isovist Min Radial       9.041362e-01 1.445549e+00   66
#> Isovist Occlusivity      3.736115e+00 5.765966e+00   66
#> Isovist Perimeter        2.389084e+01 2.472556e+01   66
#> dimension(s):
#>   from to offset delta x/y
#> x    1 13   1.75   0.5 [x]
#> y    1 12   7.25  -0.5 [y]
```
