# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

context("Agent Analysis tests")

test_that("Agent Analysis in C++", {
  lineStringMap <- st_read(
    system.file(
      "extdata", "testdata", "gallery",
      "gallery_lines.mif",
      package = "alcyon"
    ),
    geometry_column = 1L, quiet = TRUE
  )

  pointMap <- makeVGAPointMap(
    lineStringMap,
    gridSize = 0.04,
    fillX = 3.01,
    fillY = 6.7,
    maxVisibility = NA,
    boundaryGraph = FALSE,
    verbose = FALSE
  )

  Rcpp_agentAnalysis(
    pointMap@ptr,
    systemTimesteps = 3000L,
    releaseRate = 0.1,
    agentStepsToDecision = 3L,
    agentFov = 11L,
    agentLife = 1000L,
    agentLook = AgentLook$Standard,
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

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMap@ptr)
  expect_identical(dim(coords), c(4332L, length(expectedCols)))
  expect_identical(colnames(coords), expectedCols)
})

test_that("Agent Analysis in R", {
  lineStringMap <- st_read(
    system.file(
      "extdata", "testdata", "gallery",
      "gallery_lines.mif",
      package = "alcyon"
    ),
    geometry_column = 1L, quiet = TRUE
  )

  pointMap <- makeVGAPointMap(
    lineStringMap,
    gridSize = 0.04,
    fillX = 3.01,
    fillY = 6.7,
    maxVisibility = NA,
    boundaryGraph = FALSE,
    verbose = FALSE
  )


  agentAnalysis(
    pointMap,
    timesteps = 3000L,
    releaseRate = 0.1,
    agentStepsToDecision = 3L,
    agentFov = 11L,
    agentLife = 1000L,
    lookMode = AgentLook$Standard,
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

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMap@ptr)
  expect_identical(dim(coords), c(4332L, length(expectedCols)))

  expect_identical(colnames(coords), expectedCols)
})
