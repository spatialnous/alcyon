# Agent look modes.

These are meant to be used to indicate what kind of look function the
agents use to look around and decide where to go next. Possible values:

- AgentLookMode\$None

- AgentLookMode\$Standard

- AgentLookMode\$LineOfSightLength

- AgentLookMode\$OcclusionLength

- AgentLookMode\$OcclusionAny

- AgentLookMode\$OcclusionGroup45 (Occlusion group bins - 45 degrees)

- AgentLookMode\$OcclusionGroup60 (Occlusion group bins - 60 degrees)

- AgentLookMode\$OcclusionFurthest (Furthest occlusion per bin)

- AgentLookMode\$BinFarDistance (Per bin far distance weighted)

- AgentLookMode\$BinAngle (Per bin angle weighted)

- AgentLookMode\$BinFarDistanceAngle (Per bin far-distance and angle
  weighted)

- AgentLookMode\$BinMemory (Per bin memory)

## Usage

``` r
AgentLookMode
```

## Format

An object of class `list` of length 12.

## Value

A list of numbers representing each agent look mode

## Examples

``` r
AgentLookMode$Standard
#> [1] 1
AgentLookMode$LineOfSightLength
#> [1] 2
AgentLookMode$OcclusionAny
#> [1] 4
```
