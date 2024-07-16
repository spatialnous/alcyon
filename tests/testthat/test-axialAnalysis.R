# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

context("Axial Analysis tests")

test_that("Axial Analysis in C++", {
  startData <- loadSmallAxialLinesAsAxialMap(c(1L, 2L))
  shapeGraph <- attr(startData$axialMap, "sala_map")
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
  attrNameBefore <- Rcpp_ShapeMap_getAttributeNames(shapeGraph)
  expect_identical(expectedColNameBefore, attrNameBefore)

  weightBy <- Rcpp_getSfShapeMapExpectedColName(lineStringMap, 1L)
  result <- Rcpp_runAxialAnalysis(shapeGraph, c(-1.0), weightBy)
  shapeGraph <- result$mapPtr

  expectedColNameAfter <- c(
    "Ref",
    "Connectivity",
    "Data Map Ref",
    "Line Length",
    "df_1_Depthmap_Ref",
    "df_2_Connectivity",
    "df_row_name",
    "Mean Depth",
    "Node Count",
    "Integration [HH]",
    "Mean Depth [df_1_Depthmap_Ref Wgt]",
    "Total df_1_Depthmap_Ref",
    "Entropy",
    "Integration [P-value]",
    "Integration [Tekl]",
    "Intensity",
    "Harmonic Mean Depth",
    "Relativised Entropy"
  )
  attrNameBefore <- Rcpp_ShapeMap_getAttributeNames(shapeGraph)
  expect_identical(expectedColNameAfter, attrNameBefore)
})


test_that("Axial Analysis in R (non user-visible)", {
  startData <- loadSmallAxialLinesAsAxialMap(c(1L, 2L))
  shapeGraph <- startData$axialMap
  lineStringMap <- startData$sf

  weightBy <- Rcpp_getSfShapeMapExpectedColName(lineStringMap, 1L)

  shapeGraph <- axialAnalysis(
    shapeGraph,
    radii = c("n", "3"),
    includeChoice = TRUE,
    includeIntermediateMetrics = FALSE
  )

  result <- as(shapeGraph, "sf")

  expectedCols <- c(
    "Depthmap_Ref",
    "Connectivity",
    "geometry",
    "Data Map Ref",
    "Line Length",
    "Choice",
    "Choice R3",
    "Choice [Norm]",
    "Choice [Norm] R3",
    "Entropy",
    "Entropy R3",
    "Harmonic Mean Depth",
    "Harmonic Mean Depth R3",
    "Integration [HH]",
    "Integration [HH] R3",
    "Integration [P-value]",
    "Integration [P-value] R3",
    "Integration [Tekl]",
    "Integration [Tekl] R3",
    "Intensity",
    "Intensity R3",
    "Mean Depth",
    "Mean Depth R3",
    "Node Count",
    "Node Count R3",
    "Relativised Entropy",
    "Relativised Entropy R3"
  )

  expect_named(result, expectedCols)
})

test_that("Axial Analysis in R (user-visible)", {
  startData <- loadSmallAxialLinesAsAxialMap(c(1L, 2L))
  shapeGraph <- startData$axialMap
  lineStringMap <- startData$sf

  shapeGraphAnalysed <- allToAllTraverse(
    shapeGraph,
    traversalType = TraversalType$Topological,
    radii = c("n", "3"),
    includeBetweenness = TRUE
  )

  expectedCols <- c(
    "Depthmap_Ref",
    "Connectivity",
    "geometry",
    "Data Map Ref",
    "Line Length",
    "Choice",
    "Choice R3",
    "Choice [Norm]",
    "Choice [Norm] R3",
    "Entropy",
    "Entropy R3",
    "Harmonic Mean Depth",
    "Harmonic Mean Depth R3",
    "Integration [HH]",
    "Integration [HH] R3",
    "Integration [P-value]",
    "Integration [P-value] R3",
    "Integration [Tekl]",
    "Integration [Tekl] R3",
    "Intensity",
    "Intensity R3",
    "Mean Depth",
    "Mean Depth R3",
    "Node Count",
    "Node Count R3",
    "Relativised Entropy",
    "Relativised Entropy R3"
  )

  expect_named(shapeGraphAnalysed, expectedCols)
})


test_that("Local Axial Analysis in R (user-visible)", {
  startData <- loadSmallAxialLinesAsAxialMap(c(1L, 2L))
  shapeGraph <- startData$axialMap
  lineStringMap <- startData$sf

  shapeGraphAnalysed <- axialAnalysisLocal(
    shapeGraph
  )

  expectedCols <- c(
    "Depthmap_Ref",
    "Connectivity",
    "geometry",
    "Data Map Ref",
    "Line Length",
    "Control",
    "Controllability"
  )

  expect_named(shapeGraphAnalysed, expectedCols)
})
