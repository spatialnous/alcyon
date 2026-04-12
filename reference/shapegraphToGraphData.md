# Conversion of shapegraph to graph data

Creates data to be construct a graph, based on the connections and the
x,y coordinates of the centroids of shapes in a shapegraph (axial,
segment, convex). Specify weightColumn to assign weight to graph edges.

## Usage

``` r
shapegraphToGraphData(shapeGraph, weightColumn = NA)
```

## Arguments

- shapeGraph:

  A ShapeGraph

- weightColumn:

  Optional. The variable used to assign weight to graph edges

## Value

Returns a list with edges and vertices for constructing a graph.

## Details

If weightColumn is provided, edge connections weight is calculated by
taking the average of the variable of the connected nodes.

## Examples

``` r
mifFile <- system.file(
    "extdata", "testdata", "barnsbury",
    "barnsbury_small_axial_original.mif",
    package = "alcyon"
  )
  sfMap <- st_read(mifFile,
    geometry_column = 1L, quiet = TRUE
  )
  shapeGraph <- as(sfMap, "AxialShapeGraph")
shapegraphToGraphData(shapeGraph)
#> $edges
#>        from to   type        
#>   [1,] "0"  "1"  "connection"
#>   [2,] "0"  "3"  "connection"
#>   [3,] "0"  "6"  "connection"
#>   [4,] "0"  "8"  "connection"
#>   [5,] "0"  "21" "connection"
#>   [6,] "0"  "24" "connection"
#>   [7,] "1"  "2"  "connection"
#>   [8,] "1"  "4"  "connection"
#>   [9,] "1"  "7"  "connection"
#>  [10,] "1"  "13" "connection"
#>  [11,] "1"  "20" "connection"
#>  [12,] "1"  "22" "connection"
#>  [13,] "2"  "3"  "connection"
#>  [14,] "2"  "5"  "connection"
#>  [15,] "2"  "7"  "connection"
#>  [16,] "2"  "26" "connection"
#>  [17,] "2"  "27" "connection"
#>  [18,] "2"  "29" "connection"
#>  [19,] "3"  "4"  "connection"
#>  [20,] "3"  "5"  "connection"
#>  [21,] "13" "3"  "connection"
#>  [22,] "14" "3"  "connection"
#>  [23,] "20" "3"  "connection"
#>  [24,] "22" "3"  "connection"
#>  [25,] "31" "4"  "connection"
#>  [26,] "32" "4"  "connection"
#>  [27,] "5"  "9"  "connection"
#>  [28,] "11" "5"  "connection"
#>  [29,] "12" "5"  "connection"
#>  [30,] "16" "5"  "connection"
#>  [31,] "18" "5"  "connection"
#>  [32,] "36" "5"  "connection"
#>  [33,] "6"  "9"  "connection"
#>  [34,] "14" "6"  "connection"
#>  [35,] "15" "6"  "connection"
#>  [36,] "17" "6"  "connection"
#>  [37,] "19" "6"  "connection"
#>  [38,] "10" "7"  "connection"
#>  [39,] "12" "7"  "connection"
#>  [40,] "13" "7"  "connection"
#>  [41,] "39" "7"  "connection"
#>  [42,] "8"  "9"  "connection"
#>  [43,] "14" "8"  "connection"
#>  [44,] "17" "8"  "connection"
#>  [45,] "18" "8"  "connection"
#>  [46,] "19" "8"  "connection"
#>  [47,] "40" "8"  "connection"
#>  [48,] "11" "9"  "connection"
#>  [49,] "12" "9"  "connection"
#>  [50,] "16" "9"  "connection"
#>  [51,] "30" "9"  "connection"
#>  [52,] "35" "9"  "connection"
#>  [53,] "37" "9"  "connection"
#>  [54,] "41" "9"  "connection"
#>  [55,] "10" "12" "connection"
#>  [56,] "10" "25" "connection"
#>  [57,] "10" "28" "connection"
#>  [58,] "10" "42" "connection"
#>  [59,] "11" "12" "connection"
#>  [60,] "11" "23" "connection"
#>  [61,] "12" "16" "connection"
#>  [62,] "12" "33" "connection"
#>  [63,] "12" "34" "connection"
#>  [64,] "12" "35" "connection"
#>  [65,] "12" "37" "connection"
#>  [66,] "12" "44" "connection"
#>  [67,] "12" "46" "connection"
#>  [68,] "13" "19" "connection"
#>  [69,] "13" "38" "connection"
#>  [70,] "13" "49" "connection"
#>  [71,] "14" "21" "connection"
#>  [72,] "15" "17" "connection"
#>  [73,] "16" "23" "connection"
#>  [74,] "16" "25" "connection"
#>  [75,] "16" "45" "connection"
#>  [76,] "16" "50" "connection"
#>  [77,] "17" "23" "connection"
#>  [78,] "17" "30" "connection"
#>  [79,] "18" "38" "connection"
#>  [80,] "19" "38" "connection"
#>  [81,] "19" "43" "connection"
#>  [82,] "19" "47" "connection"
#>  [83,] "19" "48" "connection"
#>  [84,] "19" "52" "connection"
#>  [85,] "19" "53" "connection"
#>  [86,] "20" "31" "connection"
#>  [87,] "20" "43" "connection"
#>  [88,] "21" "47" "connection"
#>  [89,] "21" "48" "connection"
#>  [90,] "23" "25" "connection"
#>  [91,] "25" "51" "connection"
#>  [92,] "26" "27" "connection"
#>  [93,] "26" "33" "connection"
#>  [94,] "27" "34" "connection"
#>  [95,] "28" "42" "connection"
#>  [96,] "28" "45" "connection"
#>  [97,] "28" "51" "connection"
#>  [98,] "29" "54" "connection"
#>  [99,] "30" "40" "connection"
#> [100,] "30" "55" "connection"
#> [101,] "31" "32" "connection"
#> [102,] "33" "34" "connection"
#> [103,] "33" "44" "connection"
#> [104,] "34" "44" "connection"
#> [105,] "35" "56" "connection"
#> [106,] "39" "54" "connection"
#> [107,] "41" "56" "connection"
#> [108,] "42" "44" "connection"
#> [109,] "42" "50" "connection"
#> [110,] "44" "50" "connection"
#> [111,] "45" "50" "connection"
#> [112,] "45" "51" "connection"
#> [113,] "47" "48" "connection"
#> [114,] "47" "53" "connection"
#> [115,] "48" "52" "connection"
#> [116,] "49" "57" "connection"
#> [117,] "52" "55" "connection"
#> [118,] "53" "55" "connection"
#> 
#> $directed
#> [1] FALSE
#> 
#> $vertices
#>    Depthmap_Ref Connectivity Data_Map_Ref Line_Length Data Map Ref Line Length
#> 1             0            6            0   396.18808            0   396.18808
#> 2             1            7            1   259.48026            1   259.48026
#> 3             2            7            2   244.47699            2   244.47699
#> 4             3            8            3   210.34496            3   210.34496
#> 5             4            4            4   375.47971            4   375.47971
#> 6             5            8            5   145.46477            5   145.46477
#> 7             6            6            6   596.23486            6   596.23486
#> 8             7            6            7   205.18285            7   205.18285
#> 9             8            7            8   385.01169            8   385.01169
#> 10            9           10            9   273.14832            9   273.14832
#> 11           10            5           10   286.00174           10   286.00174
#> 12           11            4           11   291.54761           11   291.54761
#> 13           12           12           12   193.49419           12   193.49419
#> 14           13            6           13   213.92755           13   213.92755
#> 15           14            4           14   251.60286           14   251.60286
#> 16           15            2           15   277.79489           15   277.79489
#> 17           16            7           16   213.88315           16   213.88315
#> 18           17            5           17   162.37303           17   162.37303
#> 19           18            3           18   152.01315           18   152.01315
#> 20           19            9           19   226.00221           19   226.00221
#> 21           20            4           20   222.59605           20   222.59605
#> 22           21            4           21    75.13322           21    75.13322
#> 23           22            2           22   133.46161           22   133.46161
#> 24           23            4           23   169.04733           23   169.04733
#> 25           24            1           24    49.04080           24    49.04080
#> 26           25            4           25   109.76794           25   109.76794
#> 27           26            3           26    62.96825           26    62.96825
#> 28           27            3           27    65.30697           27    65.30697
#> 29           28            4           28    74.46476           28    74.46476
#> 30           29            2           29    44.72136           29    44.72136
#> 31           30            4           30   159.05031           30   159.05031
#> 32           31            3           31    44.38468           31    44.38468
#> 33           32            2           32    43.32436           32    43.32436
#> 34           33            4           33    65.45991           33    65.45992
#> 35           34            4           34    64.66065           34    64.66065
#> 36           35            3           35    59.53990           35    59.53990
#> 37           36            1           36    51.35173           36    51.35173
#> 38           37            2           37    24.33105           37    24.33105
#> 39           38            3           38    59.20304           38    59.20304
#> 40           39            2           39    26.40076           39    26.40076
#> 41           40            2           40    63.19810           40    63.19810
#> 42           41            2           41    61.66036           41    61.66036
#> 43           42            4           42    65.55151           42    65.55151
#> 44           43            2           43   108.04629           43   108.04629
#> 45           44            5           44   109.07337           44   109.07337
#> 46           45            4           45    75.27284           45    75.27283
#> 47           46            1           46    24.08319           46    24.08319
#> 48           47            4           47    63.82006           47    63.82006
#> 49           48            4           48    65.00000           48    65.00000
#> 50           49            2           49    31.32092           49    31.32092
#> 51           50            4           50    75.00000           50    75.00000
#> 52           51            3           51    99.02020           51    99.02020
#> 53           52            3           52   100.28459           52   100.28459
#> 54           53            3           53   101.00495           53   101.00495
#> 55           54            2           54    20.88061           54    20.88061
#> 56           55            3           55    41.04875           55    41.04875
#> 57           56            2           56    43.00000           56    43.00000
#> 58           57            1           57    36.13862           57    36.13862
#>         x       y
#> 1  1165.0 -1603.5
#> 2  1342.5 -1621.5
#> 3  1375.5 -1742.0
#> 4  1235.5 -1680.0
#> 5  1355.5 -1636.0
#> 6  1246.0 -1829.0
#> 7   989.0 -1681.0
#> 8  1361.0 -1794.0
#> 9  1118.5 -1786.5
#> 10 1124.5 -1871.5
#> 11 1353.5 -1947.0
#> 12 1217.0 -2001.0
#> 13 1263.0 -1862.0
#> 14 1273.5 -1729.0
#> 15 1103.0 -1664.0
#> 16 1016.5 -2038.5
#> 17 1253.5 -1952.5
#> 18 1069.0 -1971.5
#> 19 1187.0 -1796.0
#> 20 1086.0 -1752.5
#> 21 1258.0 -1692.5
#> 22 1054.5 -1685.0
#> 23 1281.0 -1622.0
#> 24 1203.5 -2008.0
#> 25 1290.0 -1565.5
#> 26 1306.5 -2028.0
#> 27 1304.0 -1785.5
#> 28 1293.5 -1787.0
#> 29 1330.5 -1935.0
#> 30 1333.0 -1770.0
#> 31 1067.0 -1897.5
#> 32 1189.5 -1692.5
#> 33 1175.5 -1677.0
#> 34 1308.5 -1832.0
#> 35 1300.0 -1831.5
#> 36 1194.0 -1853.5
#> 37 1230.5 -1838.0
#> 38 1173.0 -1872.0
#> 39 1187.0 -1772.5
#> 40 1356.0 -1787.5
#> 41 1091.5 -1938.5
#> 42 1155.5 -1856.5
#> 43 1329.5 -1929.0
#> 44 1154.5 -1722.5
#> 45 1306.0 -1872.5
#> 46 1279.5 -1936.5
#> 47 1275.0 -1865.0
#> 48 1072.5 -1729.0
#> 49 1045.5 -1729.0
#> 50 1224.5 -1747.0
#> 51 1281.0 -1928.5
#> 52 1307.0 -1986.5
#> 53 1040.0 -1780.5
#> 54 1079.5 -1776.5
#> 55 1340.0 -1786.0
#> 56 1064.5 -1823.0
#> 57 1175.5 -1829.0
#> 58 1216.5 -1761.5
#> 
```
