# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

context("Axial Analysis tests")

test_that("Axial Analysis in C++", {
  mod <- Rcpp::Module("alcyon_module", "alcyon")

  lineStringMap <- st_read(
    system.file(
      "extdata", "testdata", "barnsbury",
      "barnsbury_small_axial.mif",
      package = "alcyon"
    ),
    geometry_column = 1L, quiet = TRUE
  )

  shapeMap <- Rcpp_toShapeMap(lineStringMap, c(1L, 2L))
  shapeGraph <- Rcpp_toAxialShapeGraph(shapeMap)

  mod$getAxialConnections(shapeGraph)

  expectedColNameBefore <- c(
    "Ref",
    "Connectivity",
    "Line Length",
    "Data Map Ref",
    "df_1_Depthmap_Ref",
    "df_2_Choice",
    "df_row_name"
  )
  attrNameBefore <- mod$getAttributeNames(shapeGraph)
  expect_identical(expectedColNameBefore, attrNameBefore)

  weightBy <- Rcpp_getSFShapeMapExpectedColName(lineStringMap, 1L)
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
  attrNameBefore <- mod$getAttributeNames(shapeGraph)
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

  result <- axialAnalysis(
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

  expect_identical(expectedCols, names(result$data))
})
