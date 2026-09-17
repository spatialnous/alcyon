# Agent Analysis

``` r

library(alcyon)
#> Loading required package: sf
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
#> Loading required package: stars
#> Loading required package: abind
#> Registered S3 method overwritten by 'stars':
#>   method                  from
#>   st_interpolate_aw.stars sf

galleryMap <- st_read(
    system.file(
        "extdata", "testdata", "gallery",
        "gallery_lines.mif",
        package = "alcyon"
    ),
    geometry_column = 1L, quiet = TRUE
)
```

``` r

str(alcyon:::Rcpp_debugLockedColumn())
#> List of 5
#>  $ idxPlain  : num 0
#>  $ idxLocked : num 1
#>  $ numCols   : num 2
#>  $ namePlain : chr "plain"
#>  $ nameLocked: chr "locked"
```

``` r

latticeMap <- makeVGALatticeMap(
    galleryMap,
    fillX = 3.01,
    fillY = 6.7,
    gridSize = 0.06
)
plot(latticeMap["Connectivity"])
```

![](agentAnalysis_files/figure-html/unnamed-chunk-3-1.png)

``` r

agentAnalysis <- agentAnalysis(latticeMap,
    timesteps = 10000,
    releaseRate = 0.1,
    agentLifeTimesteps = 1000,
    agentFov = 16,
    agentStepsToDecision = 3,
    agentLookMode = AgentLookMode$Standard
)
plot(agentAnalysis$latticeMap["Gate Counts"])
```

![](agentAnalysis_files/figure-html/unnamed-chunk-4-1.png)

``` r

agentAnalysis <- agentAnalysis(latticeMap,
    timesteps = 10000,
    releaseRate = 0.1,
    agentLifeTimesteps = 1000,
    agentFov = 16,
    agentStepsToDecision = 3,
    agentLookMode = AgentLookMode$Standard,
    numberOfTrails = 50
)
plot(agentAnalysis$trailMap)
```

![](agentAnalysis_files/figure-html/unnamed-chunk-5-1.png)
