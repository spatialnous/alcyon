# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

context("VGA tests")

test_that("VGA in C++", {
  startData <- loadSimpleLinesAsPointMap(vector())
  lineStringMap <- startData$sf
  pointMap <- startData$pointMap

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
    "Point Second Moment"
  )

  vgaResult <- Rcpp_VGA_throughVision(pointMap@ptr)

  newExpectedCols <- "Through vision"
  expect_identical(vgaResult$newAttributes, newExpectedCols)

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMap@ptr)
  expect_identical(dim(coords), c(90L, 11L))
  expectedCols <- c(
    expectedCols,
    newExpectedCols
  )
  expect_identical(colnames(coords), expectedCols)

  vgaResult <- Rcpp_VGA_angular(pointMap@ptr, -1.0, FALSE)

  newExpectedCols <- c(
    "Angular Mean Depth",
    "Angular Total Depth",
    "Angular Node Count"
  )
  expect_identical(vgaResult$newAttributes, newExpectedCols)

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMap@ptr)
  expect_identical(dim(coords), c(90L, 14L))
  expectedCols <- c(
    expectedCols,
    newExpectedCols
  )
  expect_identical(colnames(coords), expectedCols)

  vgaResult <- Rcpp_VGA_metric(pointMap@ptr, -1.0, FALSE)

  newExpectedCols <- c(
    "Metric Mean Shortest-Path Angle",
    "Metric Mean Shortest-Path Distance",
    "Metric Mean Straight-Line Distance",
    "Metric Node Count"
  )
  expect_identical(vgaResult$newAttributes, newExpectedCols)

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMap@ptr)
  expect_identical(dim(coords), c(90L, 18L))
  expectedCols <- c(
    expectedCols,
    newExpectedCols
  )
  expect_identical(colnames(coords), expectedCols)

  vgaResult <- Rcpp_VGA_visualGlobal(pointMap@ptr, -1L, FALSE)

  newExpectedCols <- c(
    "Visual Entropy",
    "Visual Integration [HH]",
    "Visual Integration [P-value]",
    "Visual Integration [Tekl]",
    "Visual Mean Depth",
    "Visual Node Count",
    "Visual Relativised Entropy"
  )
  expect_identical(vgaResult$newAttributes, newExpectedCols)

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMap@ptr)
  expect_identical(dim(coords), c(90L, 25L))
  expectedCols <- c(
    expectedCols,
    newExpectedCols
  )
  expect_identical(colnames(coords), expectedCols)

  vgaResult <- Rcpp_VGA_visualLocal(pointMap@ptr, FALSE)

  newExpectedCols <- c(
    "Visual Clustering Coefficient",
    "Visual Control",
    "Visual Controllability"
  )
  expect_identical(vgaResult$newAttributes, newExpectedCols)

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMap@ptr)
  expect_identical(dim(coords), c(90L, 28L))
  expectedCols <- c(
    expectedCols,
    newExpectedCols
  )
  expect_identical(colnames(coords), expectedCols)

  boundaryMap <- as(lineStringMap, "ShapeMap")
  vgaResult <- Rcpp_VGA_isovist(pointMap@ptr, boundaryMap@ptr)

  newExpectedCols <- c(
    "Isovist Area",
    "Isovist Compactness",
    "Isovist Drift Angle",
    "Isovist Drift Magnitude",
    "Isovist Min Radial",
    "Isovist Max Radial",
    "Isovist Occlusivity",
    "Isovist Perimeter"
  )
  expect_identical(vgaResult$newAttributes, newExpectedCols)

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMap@ptr)
  expect_identical(dim(coords), c(90L, 36L))
  expectedCols <- c(
    expectedCols,
    newExpectedCols
  )
  expect_identical(colnames(coords), expectedCols)
})


test_that("VGA in R", {
  startData <- loadSimpleLinesAsPointMap(vector())
  lineStringMap <- startData$sf
  pointMap <- startData$pointMap

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
    "Point Second Moment"
  )

  vgaResult <- vgaThroughVision(pointMap)

  newExpectedCols <- "Through vision"
  expect_identical(vgaResult$newAttributes, newExpectedCols)

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMap@ptr)
  expect_identical(dim(coords), c(90L, 11L))
  expectedCols <- c(
    expectedCols,
    newExpectedCols
  )
  expect_identical(colnames(coords), expectedCols)

  vgaResult <- allToAllTraverse(pointMap,
    traversalType = TraversalType$Angular,
    radii = -1L,
    radiusTraversalType = TraversalType$None
  )

  newExpectedCols <- c(
    "Angular Mean Depth",
    "Angular Total Depth",
    "Angular Node Count"
  )
  expect_identical(vgaResult$newAttributes, newExpectedCols)

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMap@ptr)
  expect_identical(dim(coords), c(90L, 14L))
  expectedCols <- c(
    expectedCols,
    newExpectedCols
  )
  expect_identical(colnames(coords), expectedCols)

  vgaResult <- allToAllTraverse(pointMap,
    traversalType = TraversalType$Metric,
    radii = -1L,
    radiusTraversalType = TraversalType$None
  )

  newExpectedCols <- c(
    "Metric Mean Shortest-Path Angle",
    "Metric Mean Shortest-Path Distance",
    "Metric Mean Straight-Line Distance",
    "Metric Node Count"
  )
  expect_identical(vgaResult$newAttributes, newExpectedCols)

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMap@ptr)
  expect_identical(dim(coords), c(90L, 18L))
  expectedCols <- c(
    expectedCols,
    newExpectedCols
  )
  expect_identical(colnames(coords), expectedCols)

  vgaResult <- allToAllTraverse(pointMap,
    traversalType = TraversalType$Topological,
    radii = -1L,
    radiusTraversalType = TraversalType$None
  )

  newExpectedCols <- c(
    "Visual Entropy",
    "Visual Integration [HH]",
    "Visual Integration [P-value]",
    "Visual Integration [Tekl]",
    "Visual Mean Depth",
    "Visual Node Count",
    "Visual Relativised Entropy"
  )
  expect_identical(vgaResult$newAttributes, newExpectedCols)

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMap@ptr)
  expect_identical(dim(coords), c(90L, 25L))
  expectedCols <- c(
    expectedCols,
    newExpectedCols
  )
  expect_identical(colnames(coords), expectedCols)

  vgaResult <- vgaVisualLocal(pointMap, FALSE)

  newExpectedCols <- c(
    "Visual Clustering Coefficient",
    "Visual Control",
    "Visual Controllability"
  )
  expect_identical(vgaResult$newAttributes, newExpectedCols)

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMap@ptr)
  expect_identical(dim(coords), c(90L, 28L))
  expectedCols <- c(
    expectedCols,
    newExpectedCols
  )
  expect_identical(colnames(coords), expectedCols)

  boundaryMap <- as(lineStringMap, "ShapeMap")
  vgaResult <- vgaIsovist(pointMap, boundaryMap)

  newExpectedCols <- c(
    "Isovist Area",
    "Isovist Compactness",
    "Isovist Drift Angle",
    "Isovist Drift Magnitude",
    "Isovist Min Radial",
    "Isovist Max Radial",
    "Isovist Occlusivity",
    "Isovist Perimeter"
  )
  expect_identical(vgaResult$newAttributes, newExpectedCols)

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMap@ptr)
  expect_identical(dim(coords), c(90L, 36L))
  expectedCols <- c(
    expectedCols,
    newExpectedCols
  )
  expect_identical(colnames(coords), expectedCols)
})
