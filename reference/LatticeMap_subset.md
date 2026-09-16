# Subset LatticeMap objects

Subsetting LatticeMap objects essentially passes the data to stars See
[stars_subset](https://r-spatial.github.io/stars/reference/stars_subset.html)

## Usage

``` r
# S3 method for class 'LatticeMap'
x[...]

# S3 method for class 'LatticeMap'
x[i] <- value
```

## Arguments

- x:

  object of class `LatticeMap` passed to `stars[]`

- ...:

  other parameters passed to `stars[]`

- i:

  selector passed to `stars[] <- `

- value:

  value to be passed to `stars[] <- `
