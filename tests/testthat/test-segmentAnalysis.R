# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

context("Segment Analysis tests")

test_that("Segment Analysis in C++", {
  lineStringMap <- st_read(
    system.file(
      "extdata", "testdata", "barnsbury",
      "barnsbury_small_axial.mif",
      package = "alcyon"
    ),
    geometry_column = 1L, quiet = TRUE
  )

  segmentGraph <- sfToSegmentShapeGraph(
    lineStringMap,
    keepAttributes = c(1L, 2L),
    throughAxial = TRUE
  )

  expectedColNameBefore <- c(
    "Ref",
    "Axial Line Ref",
    "Segment Length",
    "Angular Connectivity",
    "Connectivity",
    "Axial Connectivity",
    "Axial Line Length",
    "Axial Data Map Ref",
    "Axial df_1_Depthmap_Ref",
    "Axial df_2_Choice",
    "Axial df_row_name"
  )
  attrNameBefore <- Rcpp_ShapeMap_getAttributeNames(segmentGraph)
  expect_identical(expectedColNameBefore, attrNameBefore)

  weightBy <- Rcpp_getAxialToSegmentExpectedColName(
    Rcpp_getSfShapeMapExpectedColName(lineStringMap, 1L)
  )
  Rcpp_runSegmentAnalysis(
    segmentGraph,
    radii = c(-1.0, 100.0),
    radiusStepType = alcyon::Traversal$Metric,
    analysisStepType = alcyon::Traversal$Tulip,
    NULL, # weightedMeasureColName
    TRUE, # includeChoice
    1024L, # tulipBins
    FALSE, # verbose
    FALSE, # selOnly
    FALSE # progress
  )

  testthat::expect_identical(
    dim(Rcpp_ShapeMap_getShapesAsLineCoords(segmentGraph)),
    c(293L, 4L)
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
  attrNameBefore <- Rcpp_ShapeMap_getAttributeNames(segmentGraph)
  expect_identical(expectedColNameAfter, attrNameBefore)

  connections <- Rcpp_ShapeGraph_getSegmentConnections(segmentGraph)
  expect_length(connections$from, 1392L)
  expect_length(connections$to, 1392L)
})


test_that("Segment Analysis in R", {
  lineStringMap <- st_read(
    system.file(
      "extdata", "testdata", "barnsbury", "barnsbury_small_segment.mif",
      package = "alcyon"
    ),
    geometry_column = 1L, quiet = TRUE
  )

  segmentGraph <- sfToSegmentShapeGraph(
    lineStringMap,
    keepAttributes = 5L,
    throughAxial = FALSE
  )

  weightBy <- Rcpp_getSfShapeMapExpectedColName(lineStringMap, 5L)

  segmentAnalysis(
    segmentGraph,
    radii = c("n", "100"),
    radiusStepType = Traversal$Metric,
    analysisStepType = Traversal$Tulip,
    weightWithColumn = weightBy,
    includeChoice = TRUE,
    tulipBins = 1024L,
    verbose = FALSE,
    selOnly = FALSE,
    progress = FALSE
  )

  result <- segmentShapeGraphToSf(segmentGraph)

  expectedCols <- c(
    "Ref",
    "Axial Line Ref",
    "Segment Length",
    "Data Map Ref",
    "df_5_Segment_Length",
    "df_row_name",
    "Angular Connectivity",
    "Connectivity",
    "T1024 Choice R100.00 metric",
    "T1024 Choice [df_5_Segment_Length Wgt] R100.00 metric",
    "T1024 Integration R100.00 metric",
    "T1024 Node Count R100.00 metric",
    "T1024 Total Depth R100.00 metric",
    "T1024 Integration [df_5_Segment_Length Wgt] R100.00 metric",
    "T1024 Total Depth [df_5_Segment_Length Wgt] R100.00 metric",
    "T1024 Total df_5_Segment_Length R100.00 metric",
    "T1024 Choice",
    "T1024 Choice [df_5_Segment_Length Wgt]",
    "T1024 Integration",
    "T1024 Node Count",
    "T1024 Total Depth",
    "T1024 Integration [df_5_Segment_Length Wgt]",
    "T1024 Total Depth [df_5_Segment_Length Wgt]",
    "T1024 Total df_5_Segment_Length",
    "geometry"
  )

  expect_named(result$map, expectedCols)
})

test_that("Segment Analysis in R through Axial from an sf linestring map", {
  lineStringMap <- st_read(
    system.file(
      "extdata", "testdata", "barnsbury", "barnsbury_small_axial.mif",
      package = "alcyon"
    ),
    geometry_column = 1L, quiet = TRUE
  )

  result <- segmentAnalysisSf(
    lineStringMap,
    radii = c("n", "100"),
    radiusStepType = Traversal$Metric,
    analysisStepType = Traversal$Tulip,
    weightWithColumn = names(lineStringMap)[[8L]], # Line_Length
    includeChoice = TRUE,
    keepAttributes = NULL, # the process should keep 8 as it's the weight
    throughAxial = TRUE,
    tulipBins = 1024L,
    verbose = FALSE,
    selOnly = FALSE,
    progress = FALSE
  )

  expectedCols <- c(
    "Ref",
    "Axial Line Ref",
    "Segment Length",
    "Angular Connectivity",
    "Connectivity",
    "Axial Connectivity",
    "Axial Line Length",
    "Axial Data Map Ref",
    "Axial df_8_Line_Length",
    "Axial df_row_name",
    "T1024 Choice R100.00 metric",
    "T1024 Choice [Axial df_8_Line_Length Wgt] R100.00 metric",
    "T1024 Integration R100.00 metric",
    "T1024 Node Count R100.00 metric",
    "T1024 Total Depth R100.00 metric",
    "T1024 Integration [Axial df_8_Line_Length Wgt] R100.00 metric",
    "T1024 Total Depth [Axial df_8_Line_Length Wgt] R100.00 metric",
    "T1024 Total Axial df_8_Line_Length R100.00 metric",
    "T1024 Choice",
    "T1024 Choice [Axial df_8_Line_Length Wgt]",
    "T1024 Integration",
    "T1024 Node Count",
    "T1024 Total Depth",
    "T1024 Integration [Axial df_8_Line_Length Wgt]",
    "T1024 Total Depth [Axial df_8_Line_Length Wgt]",
    "T1024 Total Axial df_8_Line_Length",
    "geometry"
  )

  expect_named(result$map, expectedCols)
})
