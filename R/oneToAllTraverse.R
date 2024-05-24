# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

#' One-to-all traversal
#'
#' Runs one-to-all traversal on a map with a graph. This is applicable to:
#' \itemize{
#'   \item{PointMaps (Visibility Graph Analysis)}
#'   \item{Axial ShapeGraphs (Axial analysis)}
#'   \item{Segment ShapeGraphs (Segment analysis)}
#' }
#'
#' @param map A PointMap, Axial ShapeGraph or Segment ShapeGraph
#' @param traversalType The traversal type. See \link{TraversalType}
#' @param fromX X coordinate of the point to start the traversal from
#' @param fromY X coordinate of the point to start the traversal from
#' @param quantizationWidth Set this to use chunks of this width instead of
#' continuous values for the cost of traversal. This is equivalent to the "tulip
#' bins" for depthmapX's tulip analysis (1024 tulip bins = pi/1024
#' quantizationWidth). Only works for Segment ShapeGraphs
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
#' "oneToAllTraverse(",
#' "  pointMap,",
#' "  traversalType = TraversalType$Metric,",
#' "  fromX = 3.01,",
#' "  fromY = 6.7",
#' ")",
#' "",
#' "# Axial analysis",
#' rxLoadSmallAxialLines(),
#' "oneToAllTraverse(",
#' "  shapeGraph,",
#' "  traversalType = TraversalType$Topological,",
#' "  fromX = 1217.1,",
#' "  fromY = -1977.3",
#' ")",
#' "",
#' "# Segment analysis",
#' rxLoadSmallSegmentLines(),
#' "oneToAllTraverse(",
#' "  shapeGraph,",
#' "  traversalType = TraversalType$Topological,",
#' "  fromX = 1217.1,",
#' "  fromY = -1977.3",
#' ")")
#' @export
oneToAllTraverse <- function(map,
                             traversalType,
                             fromX,
                             fromY,
                             quantizationWidth = NA,
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
  return(oneToAllTraversePerMapType(
    map,
    traversalType,
    fromX,
    fromY,
    quantizationWidth,
    verbose
  ))
}
oneToAllTraversePerMapType <- function(map,
                                       traversalType,
                                       fromX,
                                       fromY,
                                       quantizationWidth = NA,
                                       verbose = FALSE) {

  if (inherits(map, "PointMap")) {
    return(oneToAllTraversePointMap(
      map,
      traversalType,
      fromX,
      fromY,
      quantizationWidth,
      verbose
    ))
  } else if (inherits(map, "AxialShapeGraph")) {
    return(Rcpp_axialStepDepth(map@ptr, traversalType, fromX, fromY))
  } else if (inherits(map, "SegmentShapeGraph")) {
    tulipBins <- 0L
    if (traversalType == TraversalType$Angular
        && !is.na(quantizationWidth)) {
      tulipBins <- as.integer(pi / quantizationWidth)
    }
    return(Rcpp_segmentStepDepth(
      map@ptr, traversalType,
      fromX, fromY, tulipBins
    ))
  } else {
    stop("Can only run depth on Axial or Segment ShapeGraphs and PointMaps")
  }
}

oneToAllTraversePointMap <- function(map,
                                     traversalType,
                                     fromX,
                                     fromY,
                                     quantizationWidth = NA,
                                     verbose = FALSE) {
  if (traversalType == TraversalType$Topological) {
    return(Rcpp_VGA_visualDepth(map@ptr, cbind(fromX, fromY)))
  } else if (traversalType == TraversalType$Metric) {
    return(Rcpp_VGA_metricDepth(map@ptr, cbind(fromX, fromY)))
  } else if (traversalType == TraversalType$Angular) {
    return(Rcpp_VGA_angularDepth(map@ptr, cbind(fromX, fromY)))
  }
}
