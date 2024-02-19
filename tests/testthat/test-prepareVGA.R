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

  mapRegion <- sf::st_bbox(lineStringMap)

  pointMap <- new("PointMap")
  pointMap@ptr <- Rcpp_PointMap_createFromGrid(
    mapRegion[["xmin"]],
    mapRegion[["ymin"]],
    mapRegion[["xmax"]],
    mapRegion[["ymax"]],
    0.04
  )

  boundaryMap <- sfToShapeMap(
    lineStringMap,
    keepAttributes = vector(mode = "integer")
  )

  Rcpp_PointMap_blockLines(
    pointMap = pointMap,
    boundaryMap = boundaryMap
  )

  Rcpp_PointMap_fill(
    pointMap = pointMap,
    pointCoords = matrix(c(3.01, 6.7), nrow = 1L)
  )

  Rcpp_PointMap_makeGraph(
    pointMap = pointMap,
    boundaryGraph = FALSE,
    maxVisibility = -1.0
  )

  coords <- Rcpp_PointMap_getFilledPoints(pointMap = pointMap)

  map <- SpatialPointsDataFrame(coords[, c(1L, 2L)], data = data.frame(coords))
  gridded(map) <- TRUE
  plot(map["Connectivity"])
})
