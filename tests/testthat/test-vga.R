# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

context("VGA tests")

test_that("VGA in C++", {
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

  Rcpp_VGA_throughVision(pointMap@ptr)

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMap@ptr)
  expect_identical(dim(coords), c(4332L, 11L))
  expectedCols <- c(
    expectedCols,
    "Through vision"
  )
  expect_identical(colnames(coords), expectedCols)

  Rcpp_VGA_angular(pointMap@ptr, -1.0, FALSE)

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMap@ptr)
  expect_identical(dim(coords), c(4332L, 14L))
  expectedCols <- c(
    expectedCols,
    "Angular Mean Depth",
    "Angular Total Depth",
    "Angular Node Count"
  )
  expect_identical(colnames(coords), expectedCols)

  Rcpp_VGA_metric(pointMap@ptr, -1.0, FALSE)

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMap@ptr)
  expect_identical(dim(coords), c(4332L, 18L))
  expectedCols <- c(
    expectedCols,
    "Metric Mean Shortest-Path Angle",
    "Metric Mean Shortest-Path Distance",
    "Metric Mean Straight-Line Distance",
    "Metric Node Count"
  )
  expect_identical(colnames(coords), expectedCols)

  Rcpp_VGA_visualGlobal(pointMap@ptr, -1L, FALSE)

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMap@ptr)
  expect_identical(dim(coords), c(4332L, 25L))
  expectedCols <- c(
    expectedCols,
    "Visual Entropy",
    "Visual Integration [HH]",
    "Visual Integration [P-value]",
    "Visual Integration [Tekl]",
    "Visual Mean Depth",
    "Visual Node Count",
    "Visual Relativised Entropy"
  )
  expect_identical(colnames(coords), expectedCols)

  Rcpp_VGA_visualLocal(pointMap@ptr, FALSE)

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMap@ptr)
  expect_identical(dim(coords), c(4332L, 28L))
  expectedCols <- c(
    expectedCols,
    "Visual Clustering Coefficient",
    "Visual Control",
    "Visual Controllability"
  )
  expect_identical(colnames(coords), expectedCols)

  boundaryMap <- sfToShapeMap(lineStringMap)
  Rcpp_VGA_isovist(pointMap@ptr, boundaryMap@ptr)

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMap@ptr)
  expect_identical(dim(coords), c(4332L, 36L))
  expectedCols <- c(
    expectedCols,
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


test_that("VGA in R", {
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

  vgaThroughVision(pointMap)

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMap@ptr)
  expect_identical(dim(coords), c(4332L, 11L))
  expectedCols <- c(
    expectedCols,
    "Through vision"
  )
  expect_identical(colnames(coords), expectedCols)

  vgaAngular(pointMap, -1.0, FALSE)

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMap@ptr)
  expect_identical(dim(coords), c(4332L, 14L))
  expectedCols <- c(
    expectedCols,
    "Angular Mean Depth",
    "Angular Total Depth",
    "Angular Node Count"
  )
  expect_identical(colnames(coords), expectedCols)

  vgaMetric(pointMap, -1.0, FALSE)

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMap@ptr)
  expect_identical(dim(coords), c(4332L, 18L))
  expectedCols <- c(
    expectedCols,
    "Metric Mean Shortest-Path Angle",
    "Metric Mean Shortest-Path Distance",
    "Metric Mean Straight-Line Distance",
    "Metric Node Count"
  )
  expect_identical(colnames(coords), expectedCols)

  vgaVisualGlobal(pointMap, -1L, FALSE)

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMap@ptr)
  expect_identical(dim(coords), c(4332L, 25L))
  expectedCols <- c(
    expectedCols,
    "Visual Entropy",
    "Visual Integration [HH]",
    "Visual Integration [P-value]",
    "Visual Integration [Tekl]",
    "Visual Mean Depth",
    "Visual Node Count",
    "Visual Relativised Entropy"
  )
  expect_identical(colnames(coords), expectedCols)

  vgaVisualLocal(pointMap, FALSE)

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMap@ptr)
  expect_identical(dim(coords), c(4332L, 28L))
  expectedCols <- c(
    expectedCols,
    "Visual Clustering Coefficient",
    "Visual Control",
    "Visual Controllability"
  )
  expect_identical(colnames(coords), expectedCols)

  boundaryMap <- sfToShapeMap(lineStringMap)
  vgaIsovist(pointMap, boundaryMap)

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMap@ptr)
  expect_identical(dim(coords), c(4332L, 36L))
  expectedCols <- c(
    expectedCols,
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
