# Reduce an All-line Map to two types of fewest-line maps

Reduce an All-line Map to two types of fewest-line maps

## Usage

``` r
reduceToFewest(allLineMap)
```

## Arguments

- allLineMap:

  An AllLineShapeGraph

## Value

A list with two fewest-line axial ShapeGraphs

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
allLineMap <- makeAllLineMap(
  shapeMap,
  seedX = 3.01,
  seedY = 6.7
)
reduceToFewest(allLineMap)
#> $`Fewest-Line Map (Subsets)`
#> Simple feature collection with 1 feature and 3 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 1.888668 ymin: 1.560937 xmax: 6.259336 ymax: 6.908548
#> CRS:           NA
#>   Connectivity Line Length Ref                       geometry
#> 1            0    6.906496   0 LINESTRING (1.888668 1.5609...
#> 
#> $`Fewest-Line Map (Minimal)`
#> Simple feature collection with 1 feature and 3 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 1.888668 ymin: 1.560937 xmax: 6.259336 ymax: 6.908548
#> CRS:           NA
#>   Connectivity Line Length Ref                       geometry
#> 1            0    6.906496   0 LINESTRING (1.888668 1.5609...
#> 
```
