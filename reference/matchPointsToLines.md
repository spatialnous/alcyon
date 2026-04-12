# Match points to lines

Match points to their closest line. Matches (spatial-join) points to
lines. Finds the point closest to a line. One point is attached to one
line, thus if fewer points than lines are given then some lines will
have no point attached.

## Usage

``` r
matchPointsToLines(points, lines, getIndex = FALSE)
```

## Arguments

- points:

  Points to attach.

- lines:

  Lines to attach to.

- getIndex:

  Get the index returned and not the data.

## Value

If getIndex is TRUE then the index of the points as they relate to the
matching lines are given. If not, then the data from the points
dataframe is returned.

## Examples

``` r
segmentsMif <- system.file(
    "extdata", "testdata", "barnsbury",
    "barnsbury_small_segment_original.mif",
    package = "alcyon"
)
segmentsSf <- st_read(
    segmentsMif,
    geometry_column = 1L, quiet = TRUE
)
gateCountsMif <- system.file(
    "extdata", "testdata", "barnsbury",
    "barnsbury_ped_gatecounts.mif",
    package = "alcyon"
)
gateCountsSf <- st_read(
    gateCountsMif,
    geometry_column = 1L, quiet = TRUE
)
matchPointsToLines(gateCountsSf, segmentsSf)
#> Simple feature collection with 173 features and 19 fields (with 64 geometries empty)
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 990.3315 ymin: -2033.145 xmax: 1367.995 ymax: -1542.85
#> CRS:           NA
#> First 10 features:
#>      ID Area_Name Gate_No Axial_Map LineID        X         Y mov_ped_ph
#> 1     1 Barnsbury       1 Barnsbury      3 1017.248 -1669.215        104
#> 2     2 Barnsbury       2 Barnsbury      3 1084.500 -1639.305         88
#> 3     3 Barnsbury       3 Barnsbury      3 1144.725 -1612.515        114
#> 5     5 Barnsbury       7 Barnsbury      3 1244.630 -1568.085        134
#> 7     7 Barnsbury       9 Barnsbury      3 1301.365 -1542.850        169
#> 100 100 Barnsbury     126 Barnsbury      4 1327.140 -1569.105        163
#> 101 101 Barnsbury     127 Barnsbury      4 1341.720 -1618.845        168
#> 102 102 Barnsbury     128 Barnsbury      4 1352.345 -1655.075        133
#> 103 103 Barnsbury     129 Barnsbury      4 1361.620 -1686.715        142
#> NA   NA      <NA>      NA      <NA>     NA       NA        NA         NA
#>         landuse trafmanage streettype avebuildheight maxbuildheight
#> 1   residential     twoway    primary              3              4
#> 2   residential     twoway    primary              3              4
#> 3   residential     twoway    primary              3              4
#> 5        school     twoway    primary              4              5
#> 7   residential     twoway    primary              3              3
#> 100 residential     twoway    primary              4              5
#> 101 residential     twoway    primary              4              4
#> 102 residential     twoway    primary              3              4
#> 103 residential     twoway    primary              4              4
#> NA         <NA>       <NA>       <NA>             NA             NA
#>     diff_max_avbheight street_width pavement_width gross_veh_road_width
#> 1                    1           16            4.0                 12.0
#> 2                    1           16            4.0                 12.0
#> 3                    1           16            4.0                 12.0
#> 5                    1           12            4.0                  8.0
#> 7                    0           12            4.0                  8.0
#> 100                  1           20            5.5                 14.5
#> 101                  0           22            7.0                 15.0
#> 102                  1           21            7.0                 14.0
#> 103                  0           19            6.0                 13.0
#> NA                  NA           NA             NA                   NA
#>     parking_width effective_road_width                   geometry
#> 1             4.5                  7.5 POINT (1017.248 -1669.215)
#> 2             4.5                  7.5   POINT (1084.5 -1639.305)
#> 3             3.5                  8.5 POINT (1144.725 -1612.515)
#> 5             1.8                  6.2  POINT (1244.63 -1568.085)
#> 7             0.0                  8.0  POINT (1301.365 -1542.85)
#> 100           3.5                 11.0  POINT (1327.14 -1569.105)
#> 101           3.5                 11.5  POINT (1341.72 -1618.845)
#> 102           1.8                 12.2 POINT (1352.345 -1655.075)
#> 103           1.8                 11.2  POINT (1361.62 -1686.715)
#> NA             NA                   NA                POINT EMPTY
```
