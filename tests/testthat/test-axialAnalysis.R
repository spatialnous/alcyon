# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

context("Axial Analysis tests")

test_that("Axial Analysis in C++", {
  lineStringMap <- st_read(
    system.file(
      "extdata", "testdata", "barnsbury",
      "barnsbury_small_axial.mif",
      package = "alcyon"
    ),
    geometry_column = 1L, quiet = TRUE
  )


  shapeGraph <- sfToAxialShapeGraph(
    lineStringMap,
    keepAttributes = c(1L, 2L)
  )

  expectedColNameBefore <- c(
    "Ref",
    "Connectivity",
    "Line Length",
    "Data Map Ref",
    "df_1_Depthmap_Ref",
    "df_2_Choice",
    "df_row_name"
  )
  attrNameBefore <- Rcpp_ShapeMap_getAttributeNames(shapeGraph)
  expect_identical(expectedColNameBefore, attrNameBefore)

  weightBy <- Rcpp_getSfShapeMapExpectedColName(lineStringMap, 1L)
  Rcpp_runAxialAnalysis(shapeGraph, c(-1.0), weightBy)

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
  attrNameBefore <- Rcpp_ShapeMap_getAttributeNames(shapeGraph)
  expect_identical(expectedColNameAfter, attrNameBefore)
})


test_that("Axial Analysis in R", {
  lineStringMap <- st_read(
    system.file(
      "extdata", "testdata", "barnsbury", "barnsbury_small_axial.mif",
      package = "alcyon"
    ),
    geometry_column = 1L, quiet = TRUE
  )


  shapeGraph <- sfToAxialShapeGraph(
    lineStringMap,
    keepAttributes = c(1L, 2L)
  )
  weightBy <- Rcpp_getSfShapeMapExpectedColName(lineStringMap, 1L)

  axialAnalysis(
    shapeGraph,
    radii = c("n", "3"),
    includeChoice = TRUE,
    includeLocal = TRUE,
    includeIntermediateMetrics = FALSE
  )

  result <- axialShapeGraphToSf(shapeGraph)

  expectedCols <- c(
    "Ref",
    "Connectivity",
    "Line Length",
    "Data Map Ref",
    "df_1_Depthmap_Ref",
    "df_2_Choice",
    "df_row_name",
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
    "Relativised Entropy",
    "Control",
    "Controllability"
  )

  expect_named(result$map, c(expectedCols, "geometry"))
})

test_that("Axial Analysis in R from an sf linestring map", {
  lineStringMap <- st_read(
    system.file(
      "extdata", "testdata", "barnsbury", "barnsbury_small_axial.mif",
      package = "alcyon"
    ),
    geometry_column = 1L, quiet = TRUE
  )

  result <- axialAnalysisSf(
    lineStringMap,
    radii = c("n", "3"),
    includeChoice = TRUE,
    includeLocal = TRUE,
    includeIntermediateMetrics = FALSE
  )

  expectedCols <- c(
    "Choice",
    "Choice R3",
    "Choice [Norm]",
    "Choice [Norm] R3",
    "Control",
    "Controllability",
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

  expect_named(result$data, expectedCols)
})
