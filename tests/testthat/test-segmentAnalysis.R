# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

context("Segment Analysis tests")

test_that("Segment Analysis in C++", {
  startData <- loadSmallAxialLinesAsSegmMap(c(1L, 2L))
  lineStringMap <- startData$sf
  segmentGraph <- startData$segmentMap

  expectedColNameBefore <- c(
    "Ref",
    "Axial Line Ref",
    "Segment Length",
    "Angular Connectivity",
    "Connectivity",
    "Axial Connectivity",
    "Axial Line Length",
    "Axial Data Map Ref",
    "Axial df_row_name",
    "Axial df_1_Depthmap_Ref",
    "Axial df_2_Connectivity"
  )
  attrNameBefore <- Rcpp_ShapeMap_getAttributeNames(segmentGraph@ptr)
  expect_identical(expectedColNameBefore, attrNameBefore)

  weightBy <- Rcpp_getAxialToSegmentExpectedColName(
    Rcpp_getSfShapeMapExpectedColName(lineStringMap, 1L)
  )
  Rcpp_runSegmentAnalysis(
    segmentGraph@ptr,
    radii = c(-1.0, 100.0),
    radiusStepType = TraversalType$Metric,
    analysisStepType = TraversalType$Angular,
    NULL, # weightedMeasureColName
    TRUE, # includeChoice
    1024L, # tulipBins
    FALSE, # verbose
    FALSE, # selOnly
    FALSE # progress
  )

  expect_identical(
    dim(Rcpp_ShapeMap_getShapesAsLineCoords(segmentGraph@ptr)),
    c(178L, 4L)
  )

  expectedColNameAfter <- c(
    expectedColNameBefore,
    "T1024 Choice R100.00 metric",
    "T1024 Integration R100.00 metric",
    "T1024 Node Count R100.00 metric",
    "T1024 Total Depth R100.00 metric",
    "T1024 Choice",
    "T1024 Integration",
    "T1024 Node Count",
    "T1024 Total Depth"
  )
  attrNameBefore <- Rcpp_ShapeMap_getAttributeNames(segmentGraph@ptr)
  expect_identical(expectedColNameAfter, attrNameBefore)

  connections <- Rcpp_ShapeGraph_getSegmentConnections(segmentGraph@ptr)
  expect_length(connections$from, 770L)
  expect_length(connections$to, 770L)
})


test_that("Segment Analysis in R (non user-visible)", {
  startData <- loadSmallSegmLinesAsSegmMap(6L)
  lineStringMap <- startData$sf
  segmentGraph <- startData$segmentMap

  weightBy <- Rcpp_getSfShapeMapExpectedColName(lineStringMap, 1L)

  segmentAnalysis(
    segmentGraph,
    radii = c("n", "100"),
    radiusStepType = TraversalType$Metric,
    analysisStepType = TraversalType$Angular,
    weightWithColumn = weightBy,
    includeChoice = TRUE,
    tulipBins = 1024L,
    verbose = FALSE,
    selOnly = FALSE,
    progress = FALSE
  )

  result <- as(segmentGraph, "sf")

  expectedCols <- c(
    "Ref",
    "Axial Line Ref",
    "Segment Length",
    "Data Map Ref",
    "df_1_Segment_Length",
    "df_row_name",
    "Angular Connectivity",
    "Connectivity",
    "T1024 Choice R100.00 metric",
    "T1024 Choice [df_1_Segment_Length Wgt] R100.00 metric",
    "T1024 Integration R100.00 metric",
    "T1024 Node Count R100.00 metric",
    "T1024 Total Depth R100.00 metric",
    "T1024 Integration [df_1_Segment_Length Wgt] R100.00 metric",
    "T1024 Total Depth [df_1_Segment_Length Wgt] R100.00 metric",
    "T1024 Total df_1_Segment_Length R100.00 metric",
    "T1024 Choice",
    "T1024 Choice [df_1_Segment_Length Wgt]",
    "T1024 Integration",
    "T1024 Node Count",
    "T1024 Total Depth",
    "T1024 Integration [df_1_Segment_Length Wgt]",
    "T1024 Total Depth [df_1_Segment_Length Wgt]",
    "T1024 Total df_1_Segment_Length",
    "geometry"
  )

  expect_named(result, expectedCols)
})

test_that("Segment Analysis in R (user-visible)", {
  startData <- loadSmallAxialLinesAsSegmMap(4L)
  lineStringMap <- startData$sf
  segmentGraph <- startData$segmentMap

  weightBy <- Rcpp_getSfShapeMapExpectedColName(lineStringMap, 1L)
  weightBy <- Rcpp_getAxialToSegmentExpectedColName(weightBy)

  segmentResult <- allToAllTraverse(
    segmentGraph,
    radii = c("n", "100"),
    radiusTraversalType = TraversalType$Metric,
    traversalType = TraversalType$Angular,
    weightByAttribute = weightBy,
    includeBetweenness = TRUE,
    quantizationWidth = pi / 1024L,
    verbose = FALSE,
    progress = FALSE
  )

  newExpectedCols <- c(
    "T1024 Choice R100.00 metric",
    "T1024 Choice [Axial df_1_Line_Length Wgt] R100.00 metric",
    "T1024 Integration R100.00 metric",
    "T1024 Node Count R100.00 metric",
    "T1024 Total Depth R100.00 metric",
    "T1024 Integration [Axial df_1_Line_Length Wgt] R100.00 metric",
    "T1024 Total Depth [Axial df_1_Line_Length Wgt] R100.00 metric",
    "T1024 Total Axial df_1_Line_Length R100.00 metric",
    "T1024 Choice",
    "T1024 Choice [Axial df_1_Line_Length Wgt]",
    "T1024 Integration",
    "T1024 Node Count",
    "T1024 Total Depth",
    "T1024 Integration [Axial df_1_Line_Length Wgt]",
    "T1024 Total Depth [Axial df_1_Line_Length Wgt]",
    "T1024 Total Axial df_1_Line_Length"
  )

  expect_identical(segmentResult$newAttributes, newExpectedCols)

  result <- as(segmentGraph, "sf")

  expectedCols <- c(
    "Ref",
    "Axial Line Ref",
    "Segment Length",
    "Angular Connectivity",
    "Connectivity",
    "Axial Connectivity",
    "Axial Line Length",
    "Axial Data Map Ref",
    "Axial df_row_name",
    "Axial df_1_Line_Length",
    newExpectedCols,
    "geometry"
  )

  expect_named(result, expectedCols)
})
