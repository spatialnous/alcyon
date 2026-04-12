# Ref ID to index and vice-versa

Converts a depthmapX "Ref" ID to the indices (x, y) of a cell, or the
reverse

## Usage

``` r
refIDtoIndex(refID)

indexToRefID(i, j)
```

## Arguments

- refID:

  The Ref ID

- i:

  The x-axis index of the cell

- j:

  The y-axis index of the cell

## Value

A pair of indices (x, y) or a Ref ID

## Examples

``` r
idx <- refIDtoIndex(852645)
# outputs:
#    i   j
# 1 13 677

idx <- indexToRefID(13, 667)
# outputs:
# 852645
```
