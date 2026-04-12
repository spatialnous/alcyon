# Colour Palettes from depthmapX

Create n contiguous colours taken from depthmapX.

## Usage

``` r
depthmap.classic.colour(n, rangeMin = 0, rangeMax = 1)

depthmap.axmanesque.colour(n, rangeMin = 0, rangeMax = 1)

depthmap.purpleorange.colour(n, rangeMin = 0, rangeMax = 1)

depthmap.bluered.colour(n, rangeMin = 0, rangeMax = 1)

depthmap.grayscale.colour(n, rangeMin = 0, rangeMax = 1)

depthmap.nicehsb.colour(n, rangeMin = 0, rangeMax = 1)
```

## Arguments

- n:

  Number of colours to generate

- rangeMin:

  The min value of the range

- rangeMax:

  The max value of the range

## Value

Returns a vector of colours.

## Examples

``` r
depthmap.classic.colour(100, 0, 1)
#>   [1] "#0000FF" "#0033FF" "#0066FF" "#0099FF" "#00CCFF" "#00FFFF" "#00FFCC"
#>   [8] "#00FF99" "#00FF66" "#00FF33" "#00FF00" "#00FF00" "#11FF00" "#11FF00"
#>  [15] "#11FF00" "#22FF00" "#22FF00" "#22FF00" "#33FF00" "#33FF00" "#33FF00"
#>  [22] "#44FF00" "#44FF00" "#44FF00" "#55FF00" "#55FF00" "#55FF00" "#66FF00"
#>  [29] "#66FF00" "#66FF00" "#77FF00" "#77FF00" "#77FF00" "#88FF00" "#88FF00"
#>  [36] "#88FF00" "#99FF00" "#99FF00" "#99FF00" "#AAFF00" "#AAFF00" "#AAFF00"
#>  [43] "#BBFF00" "#BBFF00" "#BBFF00" "#CCFF00" "#CCFF00" "#CCFF00" "#DDFF00"
#>  [50] "#DDFF00" "#DDFF00" "#EEFF00" "#EEFF00" "#EEFF00" "#FFFF00" "#FFFF00"
#>  [57] "#FFFF00" "#FFEE00" "#FFEE00" "#FFEE00" "#FFDD00" "#FFDD00" "#FFDD00"
#>  [64] "#FFCC00" "#FFCC00" "#FFCC00" "#FFBB00" "#FFBB00" "#FFBB00" "#FFAA00"
#>  [71] "#FFAA00" "#FFAA00" "#FF9900" "#FF9900" "#FF9900" "#FF8800" "#FF8800"
#>  [78] "#FF8800" "#FF7700" "#FF7700" "#FF7700" "#FF6600" "#FF6600" "#FF6600"
#>  [85] "#FF5500" "#FF5500" "#FF5500" "#FF4400" "#FF4400" "#FF4400" "#FF3300"
#>  [92] "#FF3300" "#FF3300" "#FF2200" "#FF2200" "#FF2200" "#FF1100" "#FF1100"
#>  [99] "#FF1100" "#FF0000"
depthmap.axmanesque.colour(100, 0, 1)
#>   [1] "#3333DD" "#3333DD" "#3333DD" "#3333DD" "#3333DD" "#3333DD" "#3333DD"
#>   [8] "#3333DD" "#3333DD" "#3333DD" "#3333DD" "#3388DD" "#3388DD" "#3388DD"
#>  [15] "#3388DD" "#3388DD" "#3388DD" "#3388DD" "#3388DD" "#3388DD" "#3388DD"
#>  [22] "#22CCDD" "#22CCDD" "#22CCDD" "#22CCDD" "#22CCDD" "#22CCDD" "#22CCDD"
#>  [29] "#22CCDD" "#22CCDD" "#22CCDD" "#22CCBB" "#22CCBB" "#22CCBB" "#22CCBB"
#>  [36] "#22CCBB" "#22CCBB" "#22CCBB" "#22CCBB" "#22CCBB" "#22CCBB" "#22DD88"
#>  [43] "#22DD88" "#22DD88" "#22DD88" "#22DD88" "#22DD88" "#22DD88" "#22DD88"
#>  [50] "#22DD88" "#22DD88" "#88DD22" "#88DD22" "#88DD22" "#88DD22" "#88DD22"
#>  [57] "#88DD22" "#88DD22" "#88DD22" "#88DD22" "#88DD22" "#BBCC22" "#BBCC22"
#>  [64] "#BBCC22" "#BBCC22" "#BBCC22" "#BBCC22" "#BBCC22" "#BBCC22" "#BBCC22"
#>  [71] "#BBCC22" "#DDCC22" "#DDCC22" "#DDCC22" "#DDCC22" "#DDCC22" "#DDCC22"
#>  [78] "#DDCC22" "#DDCC22" "#DDCC22" "#DDCC22" "#DD8833" "#DD8833" "#DD8833"
#>  [85] "#DD8833" "#DD8833" "#DD8833" "#DD8833" "#DD8833" "#DD8833" "#DD8833"
#>  [92] "#DD3333" "#DD3333" "#DD3333" "#DD3333" "#DD3333" "#DD3333" "#DD3333"
#>  [99] "#DD3333" "#DD3333"
depthmap.purpleorange.colour(100, 0, 1)
#>   [1] "#542788" "#542788" "#542788" "#542788" "#542788" "#542788" "#542788"
#>   [8] "#542788" "#542788" "#542788" "#542788" "#542788" "#542788" "#542788"
#>  [15] "#542788" "#998EC3" "#998EC3" "#998EC3" "#998EC3" "#998EC3" "#998EC3"
#>  [22] "#998EC3" "#998EC3" "#998EC3" "#998EC3" "#998EC3" "#998EC3" "#998EC3"
#>  [29] "#998EC3" "#D8DAEB" "#D8DAEB" "#D8DAEB" "#D8DAEB" "#D8DAEB" "#D8DAEB"
#>  [36] "#D8DAEB" "#D8DAEB" "#D8DAEB" "#D8DAEB" "#D8DAEB" "#D8DAEB" "#D8DAEB"
#>  [43] "#D8DAEB" "#F7F7F7" "#F7F7F7" "#F7F7F7" "#F7F7F7" "#F7F7F7" "#F7F7F7"
#>  [50] "#F7F7F7" "#F7F7F7" "#F7F7F7" "#F7F7F7" "#F7F7F7" "#F7F7F7" "#F7F7F7"
#>  [57] "#F7F7F7" "#F7F7F7" "#FEE0B6" "#FEE0B6" "#FEE0B6" "#FEE0B6" "#FEE0B6"
#>  [64] "#FEE0B6" "#FEE0B6" "#FEE0B6" "#FEE0B6" "#FEE0B6" "#FEE0B6" "#FEE0B6"
#>  [71] "#FEE0B6" "#FEE0B6" "#F1A340" "#F1A340" "#F1A340" "#F1A340" "#F1A340"
#>  [78] "#F1A340" "#F1A340" "#F1A340" "#F1A340" "#F1A340" "#F1A340" "#F1A340"
#>  [85] "#F1A340" "#F1A340" "#B35806" "#B35806" "#B35806" "#B35806" "#B35806"
#>  [92] "#B35806" "#B35806" "#B35806" "#B35806" "#B35806" "#B35806" "#B35806"
#>  [99] "#B35806" "#B35806"
depthmap.bluered.colour(100, 0, 1)
#>   [1] "#4575B4" "#4575B4" "#4575B4" "#4575B4" "#4575B4" "#4575B4" "#4575B4"
#>   [8] "#4575B4" "#4575B4" "#4575B4" "#4575B4" "#4575B4" "#4575B4" "#4575B4"
#>  [15] "#4575B4" "#91BFDB" "#91BFDB" "#91BFDB" "#91BFDB" "#91BFDB" "#91BFDB"
#>  [22] "#91BFDB" "#91BFDB" "#91BFDB" "#91BFDB" "#91BFDB" "#91BFDB" "#91BFDB"
#>  [29] "#91BFDB" "#E0F3F8" "#E0F3F8" "#E0F3F8" "#E0F3F8" "#E0F3F8" "#E0F3F8"
#>  [36] "#E0F3F8" "#E0F3F8" "#E0F3F8" "#E0F3F8" "#E0F3F8" "#E0F3F8" "#E0F3F8"
#>  [43] "#E0F3F8" "#FFFFBF" "#FFFFBF" "#FFFFBF" "#FFFFBF" "#FFFFBF" "#FFFFBF"
#>  [50] "#FFFFBF" "#FFFFBF" "#FFFFBF" "#FFFFBF" "#FFFFBF" "#FFFFBF" "#FFFFBF"
#>  [57] "#FFFFBF" "#FFFFBF" "#FEE090" "#FEE090" "#FEE090" "#FEE090" "#FEE090"
#>  [64] "#FEE090" "#FEE090" "#FEE090" "#FEE090" "#FEE090" "#FEE090" "#FEE090"
#>  [71] "#FEE090" "#FEE090" "#FC8D59" "#FC8D59" "#FC8D59" "#FC8D59" "#FC8D59"
#>  [78] "#FC8D59" "#FC8D59" "#FC8D59" "#FC8D59" "#FC8D59" "#FC8D59" "#FC8D59"
#>  [85] "#FC8D59" "#FC8D59" "#D73027" "#D73027" "#D73027" "#D73027" "#D73027"
#>  [92] "#D73027" "#D73027" "#D73027" "#D73027" "#D73027" "#D73027" "#D73027"
#>  [99] "#D73027" "#D73027"
depthmap.grayscale.colour(100, 0, 1)
#>   [1] "#000000" "#000000" "#000000" "#000000" "#000000" "#000000" "#000000"
#>   [8] "#000000" "#000000" "#000000" "#000000" "#000000" "#000000" "#000000"
#>  [15] "#000000" "#444444" "#444444" "#444444" "#444444" "#444444" "#444444"
#>  [22] "#444444" "#444444" "#444444" "#444444" "#444444" "#444444" "#444444"
#>  [29] "#444444" "#777777" "#777777" "#777777" "#777777" "#777777" "#777777"
#>  [36] "#777777" "#777777" "#777777" "#777777" "#777777" "#777777" "#777777"
#>  [43] "#777777" "#AAAAAA" "#AAAAAA" "#AAAAAA" "#AAAAAA" "#AAAAAA" "#AAAAAA"
#>  [50] "#AAAAAA" "#AAAAAA" "#AAAAAA" "#AAAAAA" "#AAAAAA" "#AAAAAA" "#AAAAAA"
#>  [57] "#AAAAAA" "#AAAAAA" "#CCCCCC" "#CCCCCC" "#CCCCCC" "#CCCCCC" "#CCCCCC"
#>  [64] "#CCCCCC" "#CCCCCC" "#CCCCCC" "#CCCCCC" "#CCCCCC" "#CCCCCC" "#CCCCCC"
#>  [71] "#CCCCCC" "#CCCCCC" "#EEEEEE" "#EEEEEE" "#EEEEEE" "#EEEEEE" "#EEEEEE"
#>  [78] "#EEEEEE" "#EEEEEE" "#EEEEEE" "#EEEEEE" "#EEEEEE" "#EEEEEE" "#EEEEEE"
#>  [85] "#EEEEEE" "#EEEEEE" "#FFFFFF" "#FFFFFF" "#FFFFFF" "#FFFFFF" "#FFFFFF"
#>  [92] "#FFFFFF" "#FFFFFF" "#FFFFFF" "#FFFFFF" "#FFFFFF" "#FFFFFF" "#FFFFFF"
#>  [99] "#FFFFFF" "#FFFFFF"
depthmap.nicehsb.colour(100, 0, 1)
#>   [1] "#3333DD" "#3333DD" "#3333DD" "#3333DD" "#3333DD" "#3333DD" "#3333DD"
#>   [8] "#3333DD" "#3333DD" "#3333DD" "#3333DD" "#3377DD" "#3377DD" "#3377DD"
#>  [15] "#3377DD" "#3377DD" "#3377DD" "#3377DD" "#3377DD" "#3377DD" "#3377DD"
#>  [22] "#33BBDD" "#33BBDD" "#33BBDD" "#33BBDD" "#33BBDD" "#33BBDD" "#33BBDD"
#>  [29] "#33BBDD" "#33BBDD" "#33BBDD" "#33DDBB" "#33DDBB" "#33DDBB" "#33DDBB"
#>  [36] "#33DDBB" "#33DDBB" "#33DDBB" "#33DDBB" "#33DDBB" "#33DDBB" "#33DD55"
#>  [43] "#33DD55" "#33DD55" "#33DD55" "#33DD55" "#33DD55" "#33DD55" "#33DD55"
#>  [50] "#33DD55" "#33DD55" "#55DD33" "#55DD33" "#55DD33" "#55DD33" "#55DD33"
#>  [57] "#55DD33" "#55DD33" "#55DD33" "#55DD33" "#55DD33" "#BBDD33" "#BBDD33"
#>  [64] "#BBDD33" "#BBDD33" "#BBDD33" "#BBDD33" "#BBDD33" "#BBDD33" "#BBDD33"
#>  [71] "#BBDD33" "#DDBB33" "#DDBB33" "#DDBB33" "#DDBB33" "#DDBB33" "#DDBB33"
#>  [78] "#DDBB33" "#DDBB33" "#DDBB33" "#DDBB33" "#DD7733" "#DD7733" "#DD7733"
#>  [85] "#DD7733" "#DD7733" "#DD7733" "#DD7733" "#DD7733" "#DD7733" "#DD7733"
#>  [92] "#DD3333" "#DD3333" "#DD3333" "#DD3333" "#DD3333" "#DD3333" "#DD3333"
#>  [99] "#DD3333" "#DD3333"
```
