# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

#' One-to-one traversal
#'
#' Runs one-to-one traversal on a map with a graph. This is applicable to:
#' \itemize{
#'   \item{PointMaps (Visibility Graph Analysis)}
#'   \item{Segment ShapeGraphs (Segment analysis)}
#' }
#'
#' @param map A PointMap or Segment ShapeGraph
#' @param traversalType The traversal type. See \link{TraversalType}
#' @param fromX X coordinate of the point(s) to start the traversal from
#' @param fromY X coordinate of the point(s) to start the traversal from
#' @param toX X coordinate of the point(s) to start the traversal from
#' @param toY X coordinate of the point(s) to start the traversal from
#' @param quantizationWidth Set this to use chunks of this width instead of
#' continuous values for the cost of traversal. This is equivalent to the "tulip
#' bins" for depthmapX's tulip analysis (1024 tulip bins = pi/1024
#' quantizationWidth). Only works for Segment ShapeGraphs
#' @param copyMap Optional. Copy the internal sala map
#' @param verbose Optional. Show more information of the process.
#'
#' @returns Returns a list with:
#' \itemize{
#'   \item{completed: Whether the analysis completed}
#'   \item{newAttributes: The new attributes that were created during the
#'   process}
#' }
#' @eval c("@examples",
#' "# Pointmap analysis (VGA)",
#' rxLoadSimpleLinesAsPointMap(),
#' "oneToOneTraverse(",
#' "  pointMap,",
#' "  traversalType = TraversalType$Metric,",
#' "  fromX = 7.52,",
#' "  fromY = 6.02,",
#' "  toX = 5.78,",
#' "  toY = 2.96",
#' ")",
#' "",
#' "# Segment analysis",
#' rxLoadSmallSegmentLines(),
#' "oneToOneTraverse(",
#' "  shapeGraph,",
#' "  traversalType = TraversalType$Topological,",
#' "  fromX = 1217.1,",
#' "  fromY = -1977.3,",
#' "  toX = 1017.8,",
#' "  toY = -1699.3",
#' ")")
#' @export
oneToOneTraverse <- function(map,
                             traversalType,
                             fromX,
                             fromY,
                             toX,
                             toY,
                             quantizationWidth = NA,
                             copyMap = TRUE,
                             verbose = FALSE) {
  if (!(traversalType %in% as.list(TraversalType))) {
    stop("Unknown traversalType: ", traversalType)
  }

  if (!is.na(quantizationWidth) && !inherits(map, "SegmentShapeGraph")) {
    stop("quantizationWidth can only be used with Segment ShapeGraphs")
  }

  if (is.na(quantizationWidth)
      && inherits(map, "SegmentShapeGraph")
      && traversalType == TraversalType$Angular) {
    stop("Angular traversal requires a quantizationWidth")
  }
  return(oneToOneTraversePerMapType(
    map,
    traversalType,
    fromX,
    fromY,
    toX,
    toY,
    quantizationWidth,
    copyMap = copyMap,
    verbose = verbose
  ))
}
oneToOneTraversePerMapType <- function(map,
                                       traversalType,
                                       fromX,
                                       fromY,
                                       toX,
                                       toY,
                                       quantizationWidth = NA,
                                       copyMap = TRUE,
                                       verbose = FALSE) {

  if (inherits(map, "PointMap")) {
    return(oneToOneTraversePointMap(
      map,
      traversalType,
      fromX,
      fromY,
      toX,
      toY,
      quantizationWidth,
      copyMap = copyMap,
      verbose
    ))
  } else if (inherits(map, "AxialShapeGraph")) {
    stop("Shortest paths are not available for Axial ShapeGraphs")
  } else if (inherits(map, "SegmentShapeGraph")) {
    tulipBins <- 0L
    if (traversalType == TraversalType$Angular
        && !is.na(quantizationWidth)) {
      tulipBins <- as.integer(pi / quantizationWidth)
    }
    result <- Rcpp_segmentShortestPath(attr(map, "sala_map"),
                                       traversalType,
                                       cbind(fromX, fromY),
                                       cbind(toX, toY),
                                       tulipBins,
                                       copyMapNV = copyMap)
    return(processShapeMapResult(map, result))
  } else {
    stop("Can only run depth on Axial or Segment ShapeGraphs and PointMaps")
  }
}

oneToOneTraversePointMap <- function(map,
                                     traversalType,
                                     fromX,
                                     fromY,
                                     toX,
                                     toY,
                                     quantizationWidth = NA,
                                     copyMap = TRUE,
                                     verbose = FALSE) {
  if (traversalType == TraversalType$Topological) {
    result <- Rcpp_VGA_visualShortestPath(
      attr(map, "sala_map"),
      cbind(fromX, fromY),
      cbind(toX, toY),
      copyMapNV = copyMap
    )
    return(processPointMapResult(map, result))
  } else if (traversalType == TraversalType$Metric) {
    result <- Rcpp_VGA_metricShortestPath(
      attr(map, "sala_map"),
      cbind(fromX, fromY),
      cbind(toX, toY),
      copyMapNV = copyMap
    )
    return(processPointMapResult(map, result))
  } else if (traversalType == TraversalType$Angular) {
    result <- Rcpp_VGA_angularShortestPath(
      attr(map, "sala_map"),
      cbind(fromX, fromY),
      cbind(toX, toY),
      copyMapNV = copyMap
    )
    return(processPointMapResult(map, result))
  }
}
