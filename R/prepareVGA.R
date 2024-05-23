# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

#' Create a PointMap through a grid
#'
#' @param minX Minimum X of the bounding region
#' @param minY Minimum Y of the bounding region
#' @param maxX Maximum X of the bounding region
#' @param maxY Maximum Y of the bounding region
#' @param gridSize Size of the cells
#' @param verbose Optional. Show more information of the process.
#' @returns A new PointMap
#' @eval c("@examples",
#' rxLoadSimpleLinesAsShapeMap(),
#' "lineStringMap <- as(sfMap, \"sf\")",
#' "mapRegion <- sf::st_bbox(lineStringMap)",
#' "createGrid(",
#' "  minX = mapRegion[[\"xmin\"]],",
#' "  minY = mapRegion[[\"ymin\"]],",
#' "  maxX = mapRegion[[\"xmax\"]],",
#' "  maxY = mapRegion[[\"ymax\"]],",
#' "  gridSize = 0.04",
#' ")")
#' @export
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

#' Block lines on a PointMap
#'
#' Takes a PointMap and a ShapeMap with lines and blocks the cells on the
#' PointMap where the lines pass.
#'
#' @param pointMap The input PointMap
#' @param lineStringMap Map of lines, either a ShapeMap, or an sf lineString map
#' @param verbose Optional. Show more information of the process.
#' @returns None
#' @eval c("@examples",
#' rxLoadSimpleLinesAsShapeMap(),
#' "lineStringMap <- as(sfMap, \"sf\")",
#' "mapRegion <- sf::st_bbox(lineStringMap)",
#' "pointMap <- createGrid(",
#' "  minX = mapRegion[[\"xmin\"]],",
#' "  minY = mapRegion[[\"ymin\"]],",
#' "  maxX = mapRegion[[\"xmax\"]],",
#' "  maxY = mapRegion[[\"ymax\"]],",
#' "  gridSize = 0.04",
#' ")",
#' "blockLines(",
#' "  pointMap = pointMap,",
#' "  lineStringMap = lineStringMap[vector()]",
#' ")")
#' @export
blockLines <- function(pointMap,
                       lineStringMap,
                       verbose = FALSE) {
  boundaryMap <- lineStringMap
  if (!inherits(lineStringMap, "ShapeMap")) {
    boundaryMap <- as(lineStringMap, "ShapeMap")
  }
  Rcpp_PointMap_blockLines(
    pointMapPtr = pointMap@ptr,
    boundaryMapPtr = boundaryMap@ptr
  )
}

#' Fill a PointMap's grid starting from one or more points
#'
#' @param pointMap The input PointMap
#' @param fillX X coordinate of the fill points
#' @param fillY Y coordinate of the fill points
#' @param verbose Optional. Show more information of the process.
#' @returns None
#' @eval c("@examples",
#' rxLoadSimpleLinesAsShapeMap(),
#' "lineStringMap <- as(sfMap, \"sf\")",
#' "mapRegion <- sf::st_bbox(lineStringMap)",
#' "pointMap <- createGrid(",
#' "  minX = mapRegion[[\"xmin\"]],",
#' "  minY = mapRegion[[\"ymin\"]],",
#' "  maxX = mapRegion[[\"xmax\"]],",
#' "  maxY = mapRegion[[\"ymax\"]],",
#' "  gridSize = 0.04",
#' ")",
#' "blockLines(",
#' "  pointMap = pointMap,",
#' "  lineStringMap = lineStringMap[vector()]",
#' ")",
#' "fillGrid(",
#' "  pointMap = pointMap,",
#' "  fillX = 3.01,",
#' "  fillY = 6.7",
#' ")")
#' @export
fillGrid <- function(pointMap,
                     fillX,
                     fillY,
                     verbose = FALSE) {
  Rcpp_PointMap_fill(
    pointMapPtr = pointMap@ptr,
    pointCoords = cbind(fillX, fillY)
  )
}

#' Create a graph between visible cells in the PointMap
#'
#' @param pointMap The input PointMap
#' @param boundaryGraph Only create a graph on the boundary cells
#' @param maxVisibility Limit how far two cells can be to be connected
#' @param verbose Optional. Show more information of the process.
#' @returns None
#' @eval c("@examples",
#' rxLoadSimpleLinesAsShapeMap(),
#' "lineStringMap <- as(sfMap, \"sf\")",
#' "mapRegion <- sf::st_bbox(lineStringMap)",
#' "pointMap <- createGrid(",
#' "  minX = mapRegion[[\"xmin\"]],",
#' "  minY = mapRegion[[\"ymin\"]],",
#' "  maxX = mapRegion[[\"xmax\"]],",
#' "  maxY = mapRegion[[\"ymax\"]],",
#' "  gridSize = 0.5",
#' ")",
#' "blockLines(",
#' "  pointMap = pointMap,",
#' "  lineStringMap = lineStringMap[vector()]",
#' ")",
#' "fillGrid(",
#' "  pointMap = pointMap,",
#' "  fillX = 3.01,",
#' "  fillY = 6.7",
#' ")",
#' "makeVGAGraph(",
#' "  pointMap = pointMap,",
#' "  boundaryGraph = FALSE,",
#' "  maxVisibility = NA",
#' ")")
#' @export
makeVGAGraph <- function(pointMap,
                         boundaryGraph = FALSE,
                         maxVisibility = NA,
                         verbose = FALSE) {
  Rcpp_PointMap_makeGraph(
    pointMapPtr = pointMap@ptr,
    boundaryGraph = boundaryGraph,
    maxVisibility = maxVisibility
  )
}

#' Create a PointMap grid, fill it and make the graph
#'
#' This is intended to be a single command to get from the lines to a PointMap
#' ready for analysis
#'
#' @param lineStringMap Map of lines, either a ShapeMap, or an sf lineString map
#' @param gridSize Size of the cells
#' @param fillX X coordinate of the fill points
#' @param fillY Y coordinate of the fill points
#' @param boundaryGraph Only create a graph on the boundary cells
#' @param maxVisibility Limit how far two cells can be to be connected
#' @param verbose Optional. Show more information of the process.
#' @returns A new PointMap
#' @eval c("@examples",
#' rxLoadSimpleLinesAsShapeMap(),
#' "makeVGAPointMap(",
#' "  sfMap,",
#' "  gridSize = 0.5,",
#' "  fillX = 3.01,",
#' "  fillY = 6.7,",
#' "  maxVisibility = NA,",
#' "  boundaryGraph = FALSE,",
#' "  verbose = FALSE",
#' ")")
#' @export
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
    lineStringMap = lineStringMap[vector()]
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

#' Unmake the graph in a PointMap
#'
#' @param pointMap The input PointMap
#' @param removeLinks Also remove the links
#' @param verbose Optional. Show more information of the process.
#' @returns None
#' @eval c("@examples",
#' rxLoadSimpleLinesAsShapeMap(),
#' "pointMap <- makeVGAPointMap(",
#' "  sfMap,",
#' "  gridSize = 0.5,",
#' "  fillX = 3.01,",
#' "  fillY = 6.7,",
#' "  maxVisibility = NA,",
#' "  boundaryGraph = FALSE,",
#' "  verbose = FALSE",
#' ")",
#' "unmakeVGAGraph(",
#' "  pointMap = pointMap,",
#' "  removeLinks = FALSE",
#' ")")
#' @export
unmakeVGAGraph <- function(pointMap,
                           removeLinks = FALSE,
                           verbose = FALSE) {
  Rcpp_PointMap_unmakeGraph(
    pointMapPtr = pointMap@ptr,
    removeLinksWhenUnmaking = removeLinks
  )
}
