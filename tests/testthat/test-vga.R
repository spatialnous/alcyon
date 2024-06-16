# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

context("VGA tests")

test_that("VGA in C++", {
  startData <- loadSimpleLinesAsPointMap(vector())
  lineStringMap <- startData$sf
  pointMapPtr <- attr(startData$pointMap, "sala_map")

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

  vgaResult <- Rcpp_VGA_throughVision(pointMapPtr)
  pointMapPtr <- vgaResult$mapPtr

  newExpectedCols <- "Through vision"
  expect_identical(vgaResult$newAttributes, newExpectedCols)

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMapPtr)
  expect_identical(dim(coords), c(90L, 11L))
  expectedCols <- c(
    expectedCols,
    newExpectedCols
  )
  expect_identical(colnames(coords), expectedCols)

  vgaResult <- Rcpp_VGA_angular(pointMapPtr, -1.0, FALSE)
  pointMapPtr <- vgaResult$mapPtr

  newExpectedCols <- c(
    "Angular Mean Depth",
    "Angular Total Depth",
    "Angular Node Count"
  )
  expect_identical(vgaResult$newAttributes, newExpectedCols)

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMapPtr)
  expect_identical(dim(coords), c(90L, 14L))
  expectedCols <- c(
    expectedCols,
    newExpectedCols
  )
  expect_identical(colnames(coords), expectedCols)

  vgaResult <- Rcpp_VGA_metric(pointMapPtr, -1.0, FALSE)
  pointMapPtr <- vgaResult$mapPtr

  newExpectedCols <- c(
    "Metric Mean Shortest-Path Angle",
    "Metric Mean Shortest-Path Distance",
    "Metric Mean Straight-Line Distance",
    "Metric Node Count"
  )
  expect_identical(vgaResult$newAttributes, newExpectedCols)

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMapPtr)
  expect_identical(dim(coords), c(90L, 18L))
  # There is some sorting here, probably from Rcpp_VGA_metric
  expectedCols <- c(
    "x",
    "y",
    "filled",
    "blocked",
    "contextfilled",
    "edge",
    "Ref",
    "Angular Mean Depth",
    "Angular Node Count",
    "Angular Total Depth",
    "Connectivity",
    "Point First Moment",
    "Point Second Moment",
    "Through vision",
    newExpectedCols
  )
  expect_identical(colnames(coords), expectedCols)

  vgaResult <- Rcpp_VGA_visualGlobal(pointMapPtr, -1L, FALSE)
  pointMapPtr <- vgaResult$mapPtr

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

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMapPtr)
  expect_identical(dim(coords), c(90L, 25L))
  expectedCols <- c(
    "x",
    "y",
    "filled",
    "blocked",
    "contextfilled",
    "edge",
    "Ref",
    "Angular Mean Depth",
    "Angular Node Count",
    "Angular Total Depth",
    "Connectivity",
    "Metric Mean Shortest-Path Angle",
    "Metric Mean Shortest-Path Distance",
    "Metric Mean Straight-Line Distance",
    "Metric Node Count",
    "Point First Moment",
    "Point Second Moment",
    "Through vision",
    newExpectedCols
  )
  expect_identical(colnames(coords), expectedCols)

  vgaResult <- Rcpp_VGA_visualLocal(pointMapPtr, FALSE)
  pointMapPtr <- vgaResult$mapPtr

  newExpectedCols <- c(
    "Visual Clustering Coefficient",
    "Visual Control",
    "Visual Controllability"
  )
  expect_identical(vgaResult$newAttributes, newExpectedCols)

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMapPtr)
  expect_identical(dim(coords), c(90L, 28L))
  expectedCols <- c(
    expectedCols,
    newExpectedCols
  )
  expect_identical(colnames(coords), expectedCols)

  boundaryMap <- as(lineStringMap, "ShapeMap")
  vgaResult <- Rcpp_VGA_isovist(pointMapPtr, attr(boundaryMap, "sala_map"))
  pointMapPtr <- vgaResult$mapPtr

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

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMapPtr)
  expect_identical(dim(coords), c(90L, 36L))
  expectedCols <- c(
    "x",
    "y",
    "filled",
    "blocked",
    "contextfilled",
    "edge",
    "Ref",
    "Angular Mean Depth",
    "Angular Node Count",
    "Angular Total Depth",
    "Connectivity",
    "Metric Mean Shortest-Path Angle",
    "Metric Mean Shortest-Path Distance",
    "Metric Mean Straight-Line Distance",
    "Metric Node Count",
    "Point First Moment",
    "Point Second Moment",
    "Through vision",
    "Visual Clustering Coefficient",
    "Visual Control",
    "Visual Controllability",
    "Visual Entropy",
    "Visual Integration [HH]",
    "Visual Integration [P-value]",
    "Visual Integration [Tekl]",
    "Visual Mean Depth",
    "Visual Node Count",
    "Visual Relativised Entropy",
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

  pointMap <- vgaThroughVision(pointMap)

  coords <- Rcpp_PointMap_getFilledPoints(
    pointMapPtr = attr(pointMap, "sala_map")
  )
  expect_identical(dim(coords), c(90L, 11L))
  expectedCols <- c(
    expectedCols,
    "Through vision"
  )
  expect_identical(colnames(coords), expectedCols)

  pointMap <- allToAllTraverse(
    pointMap,
    traversalType = TraversalType$Angular,
    radii = -1L,
    radiusTraversalType = TraversalType$None
  )

  coords <- Rcpp_PointMap_getFilledPoints(
    pointMapPtr = attr(pointMap, "sala_map")
  )
  expect_identical(dim(coords), c(90L, 14L))
  expectedCols <- c(
    expectedCols,
    "Angular Mean Depth",
    "Angular Total Depth",
    "Angular Node Count"
  )
  expect_identical(colnames(coords), expectedCols)

  pointMap <- allToAllTraverse(
    pointMap,
    traversalType = TraversalType$Metric,
    radii = -1L,
    radiusTraversalType = TraversalType$None
  )

  coords <- Rcpp_PointMap_getFilledPoints(
    pointMapPtr = attr(pointMap, "sala_map")
  )
  expect_identical(dim(coords), c(90L, 18L))
  expectedCols <- c(
    "x",
    "y",
    "filled",
    "blocked",
    "contextfilled",
    "edge",
    "Ref",
    "Angular Mean Depth",
    "Angular Node Count",
    "Angular Total Depth",
    "Connectivity",
    "Point First Moment",
    "Point Second Moment",
    "Through vision",
    "Metric Mean Shortest-Path Angle",
    "Metric Mean Shortest-Path Distance",
    "Metric Mean Straight-Line Distance",
    "Metric Node Count"
  )
  expect_identical(colnames(coords), expectedCols)

  pointMap <- allToAllTraverse(
    pointMap,
    traversalType = TraversalType$Topological,
    radii = -1L,
    radiusTraversalType = TraversalType$None
  )

  coords <- Rcpp_PointMap_getFilledPoints(
    pointMapPtr = attr(pointMap, "sala_map")
  )
  expect_identical(dim(coords), c(90L, 25L))
  expectedCols <- c(
    "x",
    "y",
    "filled",
    "blocked",
    "contextfilled",
    "edge",
    "Ref",
    "Angular Mean Depth",
    "Angular Node Count",
    "Angular Total Depth",
    "Connectivity",
    "Metric Mean Shortest-Path Angle",
    "Metric Mean Shortest-Path Distance",
    "Metric Mean Straight-Line Distance",
    "Metric Node Count",
    "Point First Moment",
    "Point Second Moment",
    "Through vision",
    "Visual Entropy",
    "Visual Integration [HH]",
    "Visual Integration [P-value]",
    "Visual Integration [Tekl]",
    "Visual Mean Depth",
    "Visual Node Count",
    "Visual Relativised Entropy"
  )
  expect_identical(colnames(coords), expectedCols)

  pointMap <- vgaVisualLocal(pointMap, FALSE)

  coords <- Rcpp_PointMap_getFilledPoints(
    pointMapPtr = attr(pointMap, "sala_map")
  )
  expect_identical(dim(coords), c(90L, 28L))
  expectedCols <- c(
    expectedCols,
    "Visual Clustering Coefficient",
    "Visual Control",
    "Visual Controllability"
  )
  expect_identical(colnames(coords), expectedCols)

  boundaryMap <- as(lineStringMap, "ShapeMap")
  pointMap <- vgaIsovist(pointMap, boundaryMap)

  coords <- Rcpp_PointMap_getFilledPoints(
    pointMapPtr = attr(pointMap, "sala_map")
  )
  expect_identical(dim(coords), c(90L, 36L))
  expectedCols <- c(
    "x",
    "y",
    "filled",
    "blocked",
    "contextfilled",
    "edge",
    "Ref",
    "Angular Mean Depth",
    "Angular Node Count",
    "Angular Total Depth",
    "Connectivity",
    "Metric Mean Shortest-Path Angle",
    "Metric Mean Shortest-Path Distance",
    "Metric Mean Straight-Line Distance",
    "Metric Node Count",
    "Point First Moment",
    "Point Second Moment",
    "Through vision",
    "Visual Clustering Coefficient",
    "Visual Control",
    "Visual Controllability",
    "Visual Entropy",
    "Visual Integration [HH]",
    "Visual Integration [P-value]",
    "Visual Integration [Tekl]",
    "Visual Mean Depth",
    "Visual Node Count",
    "Visual Relativised Entropy",
    "Isovist Area",
    "Isovist Compactness",
    "Isovist Drift Angle",
    "Isovist Drift Magnitude",
    "Isovist Min Radial",
    "Isovist Max Radial",
    "Isovist Occlusivity",
    "Isovist Perimeter"
  )
  expect_identical(colnames(coords), expectedCols)
})
