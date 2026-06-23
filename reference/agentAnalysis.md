# Agent Analysis

Runs Agent Analysis on the given LatticeMap

## Usage

``` r
agentAnalysis(
  latticeMap,
  timesteps,
  releaseRate,
  agentLifeTimesteps,
  agentFov,
  agentStepsToDecision,
  agentLookMode,
  originX = vector(),
  originY = vector(),
  locationSeed = 0L,
  numberOfTrails = 0L,
  getGateCounts = FALSE,
  copyMap = TRUE,
  verbose = FALSE,
  progress = FALSE
)
```

## Arguments

- latticeMap:

  A LatticeMap, used as an exosomatic visual map for agents to take
  exploratory information

- timesteps:

  Number of total system timesteps.

- releaseRate:

  Agent release rate (likelihood of release per timestep).

- agentLifeTimesteps:

  Agent total lifetime (in timesteps)

- agentFov:

  Agent field-of-view (out of 32 bins = 360).

- agentStepsToDecision:

  Agent steps before turn decision.

- agentLookMode:

  The agent look mode. See
  [AgentLookMode](https://spatialnous.github.io/alcyon/reference/AgentLookMode.md)

- originX:

  Agent starting points (x coordinates).

- originY:

  Agent starting point (y coordinates).

- locationSeed:

  Agents to start at random locations with specific seed (0 to 10).
  Default is 0.

- numberOfTrails:

  Record trails for this amount of agents (set to 0 to record all, with
  max possible currently = 50).

- getGateCounts:

  Get values at gates

- copyMap:

  Optional. Copy the internal sala map

- verbose:

  Optional. Show more information of the process.

- progress:

  Optional. Show process progress.

## Value

Returns a list with:

- newAttributes: The new attributes that were created during the process

- trailMap: A ShapeMap with trails if numberOfTrails was set over 0

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
agentAnalysis(
  latticeMap,
  timesteps = 3000L,
  releaseRate = 0.1,
  agentStepsToDecision = 3L,
  agentFov = 11L,
  agentLife = 1000L,
  agentLookMode = AgentLookMode$Standard,
  originX = NA,
  originY = NA,
  locationSeed = 1L,
  numberOfTrails = 50L,
  getGateCounts = FALSE,
  verbose = FALSE
)
#> $latticeMap
#> stars object with 2 dimensions and 8 attributes
#> attribute(s):
#> Warning: number of columns of result is not a multiple of vector length (arg 1)
#>                            Min.     1st Qu.     Median         Mean     3rd Qu.
#> Ref                     0.00000 196610.7500 393221.500 3.932215e+05 589832.2500
#> Connectivity           39.00000     65.0000     75.000 7.106667e+01     81.0000
#> Point First Moment     92.03807    145.1355    173.267 1.750389e+02    202.1131
#> Point Second Moment   240.00000    388.5625    531.500 5.387111e+02    665.5625
#> blocked                 0.00000      0.0000      0.000 3.205128e-01      1.0000
#> contextfilled           0.00000      0.0000      0.000 0.000000e+00      0.0000
#> filled                  0.00000      0.0000      1.000 5.769231e-01      1.0000
#> Gate Counts          1166.00000   1696.7500   2807.000 2.656367e+03   3265.7500
#>                             Max. NAs NA's
#> Ref                  786443.0000   0    0
#> Connectivity             88.0000  66    0
#> Point First Moment      292.1885  66    0
#> Point Second Moment    1157.7500  66    0
#> blocked                   1.0000   0    0
#> contextfilled             0.0000   0    0
#> filled                    1.0000   0    0
#> Gate Counts            5149.0000  66    0
#> dimension(s):
#>   from to offset delta x/y
#> x    1 13   1.75   0.5 [x]
#> y    1 12   7.25  -0.5 [y]
#> 
#> $trailMap
#> Simple feature collection with 50 features and 1 field
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 1.751471 ymin: 1.752596 xmax: 7.744374 ymax: 6.73951
#> CRS:           NA
#> First 10 features:
#>    Ref                       geometry
#> 1    0 LINESTRING (5.276393 2.4472...
#> 2    1 LINESTRING (4.341886 4.4743...
#> 3    2 LINESTRING (2.485071 5.1212...
#> 4    3 LINESTRING (4.314305 5.5357...
#> 5    4 LINESTRING (3.121268 3.5149...
#> 6    5 LINESTRING (5.040427 3.3030...
#> 7    6 LINESTRING (2.723607 4.5527...
#> 8    7 LINESTRING (2.474342 4.8418...
#> 9    8 LINESTRING (5.005025 5.9292...
#> 10   9 LINESTRING (4.014929 6.3787...
#> 
```
