# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

#' Read MetaGraph
#'
#' Reads a metagraph into a bunch of ShapeMaps/ShapeGraphs/PointMaps
#'
#' @param fileName The metagraph file
#' @returns A list of ShapeMaps, ShapeGraphs and PointMaps
#' @examples
#' fileName <- system.file(
#'   "extdata", "testdata", "barnsbury", "barnsburySmall.graph",
#'   package = "alcyon"
#' )
#' readMetaGraph(fileName)
#' @export
readMetaGraph <- function(fileName) {
  result <- list(
    shapeMaps = list(),
    shapeGraphs = list(),
    axialShapeGraphs = list(),
    alllineShapeGraphs = list(),
    segmentShapeGraphs = list(),
    pointMaps = list()
  )
  mgraphData <- Rcpp_MetaGraph_read(fileName)
  if (length(mgraphData$shapeMaps) > 0L) {
    result$shapeMaps <- lapply(mgraphData$shapeMaps, function(mapData) {
      outMap <- new("ShapeMap")
      outMap@ptr <- mapData$ptr
      outMap
    })
  }
  if (length(mgraphData$pointMaps) > 0L) {
    result$pointMaps <- lapply(mgraphData$pointMaps, function(mapData) {
      outMap <- new("PointMap")
      outMap@ptr <- mapData$ptr
      outMap
    })
  }
  for (mapData in mgraphData$shapeGraphs) {
    if (mapData$type == "axial") {
      outMap <- new("AxialShapeGraph")
      outMap@ptr <- mapData$ptr
      result$axialShapeGraphs <- c(result$axialShapeGraphs, outMap)
    } else if (mapData$type == "allline") {
      outMap <- new("AllLineShapeGraph")
      outMap@ptr <- mapData$ptr
      result$alllineShapeGraphs <- c(result$alllineShapeGraphs, outMap)
    } else if (mapData$type == "segment") {
      outMap <- new("SegmentShapeGraph")
      outMap@ptr <- mapData$ptr
      result$segmentShapeGraphs <- c(result$segmentShapeGraphs, outMap)
    } else {
      outMap <- new("ShapeGraph")
      outMap@ptr <- mapData$ptr
      result$shapeGraphs <- c(result$shapeGraphs, outMap)
    }
  }
  return(result)
}
