# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

context("Agent Analysis tests")

test_that("Agent Analysis in C++", {
  pointMap <- loadInteriorLinesAsPointMap()$pointMap

  result <- Rcpp_agentAnalysis(
    attr(pointMap, "sala_map"),
    systemTimesteps = 3000L,
    releaseRate = 0.1,
    agentStepsToDecision = 3L,
    agentFov = 11L,
    agentLife = 1000L,
    agentLookMode = AgentLookMode$Standard,
    agentReleaseLocations = cbind(1L, 1L),
    randomReleaseLocationSeed = 1L,
    recordTrailForAgents = 50L,
    getGateCounts = FALSE,
    verbose = FALSE
  )

  expectedCols <- c(
    "x",
    "y",
    "filled",
    "blocked",
    "contextfilled",
    "edge",
    "Ref",
    "Connectivity",
    "Point First Moment",
    "Point Second Moment",
    "Gate Counts"
  )

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = result$mapPtr)
  expect_identical(dim(coords), c(4332L, length(expectedCols)))
  expect_identical(colnames(coords), expectedCols)
})

test_that("Agent Analysis in R", {
  pointMap <- loadInteriorLinesAsPointMap()$pointMap

  agentAnalysis <- agentAnalysis(
    pointMap,
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

  expectedCols <- c(
    "x",
    "y",
    "filled",
    "blocked",
    "contextfilled",
    "edge",
    "Ref",
    "Connectivity",
    "Point First Moment",
    "Point Second Moment",
    "Gate Counts"
  )

  coords <- Rcpp_PointMap_getFilledPoints(
    pointMapPtr = attr(agentAnalysis$pointMap, "sala_map")
  )
  expect_identical(dim(coords), c(4332L, length(expectedCols)))

  expect_identical(colnames(coords), expectedCols)
})
