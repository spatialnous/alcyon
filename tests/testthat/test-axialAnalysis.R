# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

context("Axial Analysis tests")

test_that("Axial Analysis in C++", {
  startData <- loadSmallAxialLinesAsAxialMap(c(1L, 2L))
  shapeGraph <- startData$axialMap
  lineStringMap <- startData$sf

  expectedColNameBefore <- c(
    "Ref",
    "Connectivity",
    "Line Length",
    "Data Map Ref",
    "df_row_name",
    "df_1_Depthmap_Ref",
    "df_2_Connectivity"
  )
  attrNameBefore <- Rcpp_ShapeMap_getAttributeNames(shapeGraph@ptr)
  expect_identical(expectedColNameBefore, attrNameBefore)

  weightBy <- Rcpp_getSfShapeMapExpectedColName(lineStringMap, 1L)
  Rcpp_runAxialAnalysis(shapeGraph@ptr, c(-1.0), weightBy)

  expectedColNameAfter <- c(
    expectedColNameBefore,
    "Entropy",
    "Integration [HH]",
    "Integration [P-value]",
    "Integration [Tekl]",
    "Intensity",
    "Harmonic Mean Depth",
    "Mean Depth",
    "Node Count",
    "Relativised Entropy",
    "Mean Depth [df_1_Depthmap_Ref Wgt]",
    "Total df_1_Depthmap_Ref"
  )
  attrNameBefore <- Rcpp_ShapeMap_getAttributeNames(shapeGraph@ptr)
  expect_identical(expectedColNameAfter, attrNameBefore)
})


test_that("Axial Analysis in R (non user-visible)", {
  startData <- loadSmallAxialLinesAsAxialMap(c(1L, 2L))
  shapeGraph <- startData$axialMap
  lineStringMap <- startData$sf

  weightBy <- Rcpp_getSfShapeMapExpectedColName(lineStringMap, 1L)

  axialAnalysis(
    shapeGraph,
    radii = c("n", "3"),
    includeChoice = TRUE,
    includeIntermediateMetrics = FALSE
  )

  result <- as(shapeGraph, "sf")

  expectedCols <- c(
    "Ref",
    "Connectivity",
    "Line Length",
    "Data Map Ref",
    "df_row_name",
    "df_1_Depthmap_Ref",
    "df_2_Connectivity",
    "Choice R3",
    "Choice [Norm] R3",
    "Entropy R3",
    "Integration [HH] R3",
    "Integration [P-value] R3",
    "Integration [Tekl] R3",
    "Intensity R3",
    "Harmonic Mean Depth R3",
    "Mean Depth R3",
    "Node Count R3",
    "Relativised Entropy R3",
    "Choice",
    "Choice [Norm]",
    "Entropy",
    "Integration [HH]",
    "Integration [P-value]",
    "Integration [Tekl]",
    "Intensity",
    "Harmonic Mean Depth",
    "Mean Depth",
    "Node Count",
    "Relativised Entropy"
  )

  expect_named(result, c(expectedCols, "geometry"))
})

test_that("Axial Analysis in R (user-visible)", {
  startData <- loadSmallAxialLinesAsAxialMap(c(1L, 2L))
  shapeGraph <- startData$axialMap
  lineStringMap <- startData$sf

  axialResult <- allToAllTraverse(
    shapeGraph,
    traversalType = TraversalType$Topological,
    radii = c("n", "3"),
    includeBetweenness = TRUE
  )

  newExpectedCols <- c(
    "Choice R3",
    "Choice [Norm] R3",
    "Entropy R3",
    "Integration [HH] R3",
    "Integration [P-value] R3",
    "Integration [Tekl] R3",
    "Intensity R3",
    "Harmonic Mean Depth R3",
    "Mean Depth R3",
    "Node Count R3",
    "Relativised Entropy R3",
    "Choice",
    "Choice [Norm]",
    "Entropy",
    "Integration [HH]",
    "Integration [P-value]",
    "Integration [Tekl]",
    "Intensity",
    "Harmonic Mean Depth",
    "Mean Depth",
    "Node Count",
    "Relativised Entropy"
  )

  expect_identical(axialResult$newAttributes, newExpectedCols)

  result <- as(shapeGraph, "sf")

  expectedCols <- c(
    "Ref",
    "Connectivity",
    "Line Length",
    "Data Map Ref",
    "df_row_name",
    "df_1_Depthmap_Ref",
    "df_2_Connectivity",
    newExpectedCols
  )

  expect_named(result, c(expectedCols, "geometry"))
})


test_that("Local Axial Analysis in R (user-visible)", {
  startData <- loadSmallAxialLinesAsAxialMap(c(1L, 2L))
  shapeGraph <- startData$axialMap
  lineStringMap <- startData$sf

  axialResult <- axialAnalysisLocal(
    shapeGraph
  )

  newExpectedCols <- c(
    "Control",
    "Controllability"
  )

  expect_identical(axialResult$newAttributes, newExpectedCols)

  axialMap <- as(shapeGraph, "sf")

  expectedCols <- c(
    "Ref",
    "Connectivity",
    "Line Length",
    "Data Map Ref",
    "df_row_name",
    "df_1_Depthmap_Ref",
    "df_2_Connectivity",
    newExpectedCols
  )

  expect_named(axialMap, c(expectedCols, "geometry"))
})
