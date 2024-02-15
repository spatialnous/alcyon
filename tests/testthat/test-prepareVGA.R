# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

context("PointMap tests")

test_that("PointMaps in C++", {
  lineStringMap <- st_read(
    system.file(
      "extdata", "testdata", "gallery",
      "gallery_lines.mif",
      package = "alcyon"
    ),
    geometry_column = 1L, quiet = TRUE
  )

  boundaryMap <- sfToShapeMap(
    lineStringMap,
    keepAttributes = vector(mode = "integer")
  )

  pointMap <- alcyon:::Rcpp_PointMap_createFromGrid(
    boundaryMap,
    0.04
  )

  alcyon:::Rcpp_PointMap_fill(
    pointMap = pointMap,
    boundaryMap = boundaryMap,
    pointCoords = matrix(c(3.01, 6.7), nrow = 1L)
  )

  alcyon:::Rcpp_PointMap_makeGraph(
    pointMap = pointMap,
    boundaryMap = boundaryMap,
    boundaryGraph = FALSE,
    maxVisibility = -1.0
  )

})
