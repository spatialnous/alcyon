# Subset SegmentShapeGraph objects

Subsetting SegmentShapeGraph objects essentially passes the data to sf.
See [sf](https://r-spatial.github.io/sf/reference/sf.html)

## Usage

``` r
# S3 method for class 'SegmentShapeGraph'
x[...]

# S3 method for class 'SegmentShapeGraph'
x[...] <- value
```

## Arguments

- x:

  object of class `SegmentShapeGraph` passed to `stars[]`

- ...:

  other parameters passed to `stars[] <- `

- value:

  value to be passed to `sf[] <- `
