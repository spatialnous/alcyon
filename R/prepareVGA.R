# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

createGrid <- function(minX,
                       minY,
                       maxX,
                       maxY,
                       gridSize,
                       verbose = FALSE) {
  pointMap <- new("PointMap")
  pointMap@ptr <- Rcpp_PointMap_createFromGrid(
    minX,
    minY,
    maxX,
    maxY,
    gridSize
  )
  return(pointMap)
}

blockLines <- function(pointMap,
                       lineStringMap,
                       verbose = FALSE) {
  boundaryMap <- sfToShapeMap(
    lineStringMap,
    keepAttributes = vector(mode = "integer")
  )
  Rcpp_PointMap_blockLines(
    pointMap = pointMap@ptr,
    boundaryMap = boundaryMap
  )
}

fillGrid <- function(pointMap,
                     fillX,
                     fillY,
                     verbose = FALSE) {
  Rcpp_PointMap_fill(
    pointMap = pointMap@ptr,
    pointCoords = cbind(fillX, fillY)
  )
}

makeVGAGraph <- function(pointMap,
                         boundaryGraph = FALSE,
                         maxVisibility = NA,
                         verbose = FALSE) {
  Rcpp_PointMap_makeGraph(
    pointMap = pointMap@ptr,
    boundaryGraph = boundaryGraph,
    maxVisibility = maxVisibility
  )
}

makeVGAPointMap <- function(lineStringMap,
                            gridSize,
                            fillX,
                            fillY,
                            maxVisibility = NA,
                            boundaryGraph = FALSE,
                            verbose = FALSE) {

  mapRegion <- sf::st_bbox(lineStringMap)

  pointMap <- createGrid(
    mapRegion[["xmin"]],
    mapRegion[["ymin"]],
    mapRegion[["xmax"]],
    mapRegion[["ymax"]],
    gridSize
  )

  blockLines(
    pointMap = pointMap,
    lineStringMap = lineStringMap
  )

  fillGrid(
    pointMap = pointMap,
    fillX,
    fillY
  )

  makeVGAGraph(
    pointMap = pointMap,
    boundaryGraph = boundaryGraph,
    maxVisibility = maxVisibility
  )
  return(pointMap)
}

unmakeVGAGraph <- function(pointMap,
                           removeLinks = FALSE,
                           verbose = FALSE) {
  Rcpp_PointMap_unmakeGraph(
    pointMap = pointMap@ptr,
    removeLinksWhenUnmaking = removeLinks
  )
}
