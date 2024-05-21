# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

context("PointMap tests")

test_that("PointMaps in C++", {
  startData <- loadInteriorLinesAsShapeMap(vector())
  lineStringMap <- startData$sf
  boundaryMap <- startData$shapeMap

  mapRegion <- sf::st_bbox(lineStringMap)

  pointMap <- new("PointMap")
  pointMap@ptr <- Rcpp_PointMap_createFromGrid(
    mapRegion[["xmin"]],
    mapRegion[["ymin"]],
    mapRegion[["xmax"]],
    mapRegion[["ymax"]],
    0.04
  )


  Rcpp_PointMap_blockLines(
    pointMapPtr = pointMap@ptr,
    boundaryMap = boundaryMap@ptr
  )

  Rcpp_PointMap_fill(
    pointMapPtr = pointMap@ptr,
    pointCoords = matrix(c(3.01, 6.7), nrow = 1L)
  )

  Rcpp_PointMap_makeGraph(
    pointMapPtr = pointMap@ptr,
    boundaryGraph = FALSE,
    maxVisibility = -1.0
  )

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMap@ptr)
  expect_identical(dim(coords), c(4332L, 10L))
  expect_identical(colnames(coords), c(
    "x", "y", "filled", "blocked",
    "contextfilled", "edge", "Ref",
    "Connectivity", "Point First Moment",
    "Point Second Moment"
  ))
})

test_that("PointMaps in R", {
  lineStringMap <- loadInteriorLinesAsSf()$sf

  pointMap <- makeVGAPointMap(lineStringMap,
    gridSize = 0.04,
    fillX = 3.01,
    fillY = 6.7,
    maxVisibility = NA,
    boundaryGraph = FALSE,
    verbose = FALSE
  )

  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMap@ptr)

  expect_identical(dim(coords), c(4332L, 10L))
  expect_identical(colnames(coords), c(
    "x", "y", "filled", "blocked",
    "contextfilled", "edge", "Ref",
    "Connectivity", "Point First Moment",
    "Point Second Moment"
  ))
})
