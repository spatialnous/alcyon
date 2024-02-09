# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

context("MetaGraph tests")

test_that("Load data from MetaGraph", {
  fileName <- system.file(
    "extdata", "testdata", "barnsbury", "barnsburySmall.graph",
    package = "alcyon"
  )
  metaGraphData <- Rcpp_MetaGraph_read(fileName)
  expect_length(metaGraphData$shapeMaps, 1L)
  expect_length(metaGraphData$shapeGraphs, 1L)
  expect_length(metaGraphData$pointMaps, 0L)
})
