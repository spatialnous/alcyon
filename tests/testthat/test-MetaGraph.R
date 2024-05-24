# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

context("MetaGraph tests")

test_that("Load axial data from MetaGraph in C++", {
  fileName <- system.file(
    "extdata", "testdata", "barnsbury", "barnsburySmall.graph",
    package = "alcyon"
  )
  metaGraphData <- Rcpp_MetaGraph_read(fileName)
  expect_length(metaGraphData$shapeMaps, 1L)
  expect_length(metaGraphData$shapeGraphs, 1L)
  expect_length(metaGraphData$pointMaps, 0L)
})


test_that("Load axial data from MetaGraph in R", {
  fileName <- system.file(
    "extdata", "testdata", "barnsbury", "barnsburySmall.graph",
    package = "alcyon"
  )
  metaGraphData <- readMetaGraph(fileName)
  expect_length(metaGraphData$shapeMaps, 1L)
  expect_length(metaGraphData$shapeGraphs, 0L)
  expect_length(metaGraphData$axialShapeGraphs, 1L)
  expect_length(metaGraphData$segmentShapeGraphs, 0L)
  expect_length(metaGraphData$allLineShapeGraphs, 0L)
  expect_length(metaGraphData$pointMaps, 0L)
})

test_that("Load pointmap data from MetaGraph in C++", {
  fileName <- system.file(
    "extdata", "testdata", "simple", "simple_interior.graph",
    package = "alcyon"
  )
  metaGraphData <- Rcpp_MetaGraph_read(fileName)
  expect_length(metaGraphData$shapeMaps, 1L)
  expect_length(metaGraphData$shapeGraphs, 0L)
  expect_length(metaGraphData$pointMaps, 1L)
})


test_that("Load pointmap data from MetaGraph in R", {
  fileName <- system.file(
    "extdata", "testdata", "simple", "simple_interior.graph",
    package = "alcyon"
  )
  metaGraphData <- readMetaGraph(fileName)
  expect_length(metaGraphData$shapeMaps, 1L)
  expect_length(metaGraphData$shapeGraphs, 0L)
  expect_length(metaGraphData$axialShapeGraphs, 0L)
  expect_length(metaGraphData$segmentShapeGraphs, 0L)
  expect_length(metaGraphData$allLineShapeGraphs, 0L)
  expect_length(metaGraphData$pointMaps, 1L)
})
