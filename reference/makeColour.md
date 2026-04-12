# Single Colour from depthmapX's Palettes

Create a single colour from depthmapX's palettes.

## Usage

``` r
makeDepthmapClassicColour(value, rangeMin = 0, rangeMax = 1)

makeAxmanesqueColour(value, rangeMin = 0, rangeMax = 1)

makePurpleOrangeColour(value, rangeMin = 0, rangeMax = 1)

makeBlueRedColour(value, rangeMin = 0, rangeMax = 1)

makeGreyScaleColour(value, rangeMin = 0, rangeMax = 1)

makeNiceHSBColour(value, rangeMin = 0, rangeMax = 1)
```

## Arguments

- value:

  Value within the min/max range to take

- rangeMin:

  The min value of the range

- rangeMax:

  The max value of the range

## Value

Returns a single colour.

## Examples

``` r
makeDepthmapClassicColour(0.2, 0, 1)
#> [1] "#33FF00"
makeAxmanesqueColour(0.2, 0, 1)
#> [1] "#3388DD"
makePurpleOrangeColour(0.2, 0, 1)
#> [1] "#998EC3"
makeBlueRedColour(0.2, 0, 1)
#> [1] "#91BFDB"
makeGreyScaleColour(0.2, 0, 1)
#> [1] "#444444"
makeNiceHSBColour(0.2, 0, 1)
#> [1] "#3377DD"
```
