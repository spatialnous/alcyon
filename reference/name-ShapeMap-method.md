# Get the ShapeMap name

Get the ShapeMap name

## Usage

``` r
# S4 method for class 'ShapeMap'
name(map)
```

## Arguments

- map:

  A ShapeMap

## Value

The name of the ShapeMap as a string

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
name(shapeMap)
#> [1] "tmp_df_shp"
```
