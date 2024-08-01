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
                             copyMap = TRUE,
                             verbose = FALSE) {
    if (!(traversalType %in% as.list(TraversalType))) {
        stop("Unknown traversalType: ", traversalType)
    }

    if (!is.na(quantizationWidth) && !inherits(map, "SegmentShapeGraph")) {
        stop("quantizationWidth can only be used with Segment ShapeGraphs")
    }

    if (is.na(quantizationWidth) &&
            inherits(map, "SegmentShapeGraph") &&
            traversalType == TraversalType$Angular) {
        stop("Angular traversal requires a quantizationWidth")
    }
    return(oneToAllTraversePerMapType(
        map,
        traversalType,
        fromX,
        fromY,
        quantizationWidth,
        copyMap = copyMap,
        verbose = verbose
    ))
}
oneToAllTraversePerMapType <- function(map,
                                       traversalType,
                                       fromX,
                                       fromY,
                                       quantizationWidth = NA,
                                       copyMap = TRUE,
                                       verbose = FALSE) {
    if (inherits(map, "PointMap")) {
        return(oneToAllTraversePointMap(
            map,
            traversalType,
            fromX,
            fromY,
            quantizationWidth,
            copyMap = copyMap,
            verbose
        ))
    } else if (inherits(map, "AxialShapeGraph")) {
        result <- Rcpp_axialStepDepth(attr(map, "sala_map"),
            traversalType,
            fromX,
            fromY,
            copyMapNV = copyMap
        )
        return(processShapeMapResult(map, result))
    } else if (inherits(map, "SegmentShapeGraph")) {
        tulipBins <- 0L
        if (traversalType == TraversalType$Angular &&
                !is.na(quantizationWidth)) {
            tulipBins <- as.integer(pi / quantizationWidth)
        }
        result <- Rcpp_segmentStepDepth(attr(map, "sala_map"),
            traversalType,
            fromX,
            fromY,
            tulipBins,
            copyMapNV = copyMap
        )
        return(processShapeMapResult(map, result))
    } else {
        stop("Can only run depth on Axial or Segment ShapeGraphs and PointMaps")
    }
}

oneToAllTraversePointMap <- function(map,
                                     traversalType,
                                     fromX,
                                     fromY,
                                     quantizationWidth = NA,
                                     copyMap = TRUE,
                                     verbose = FALSE) {
    if (traversalType == TraversalType$Topological) {
        result <- Rcpp_VGA_visualDepth(
            attr(map, "sala_map"),
            cbind(fromX, fromY),
            copyMapNV = copyMap
        )
        return(processPointMapResult(map, result))
    } else if (traversalType == TraversalType$Metric) {
        result <- Rcpp_VGA_metricDepth(
            attr(map, "sala_map"),
            cbind(fromX, fromY),
            copyMapNV = copyMap
        )
        return(processPointMapResult(map, result))
    } else if (traversalType == TraversalType$Angular) {
        result <- Rcpp_VGA_angularDepth(
            attr(map, "sala_map"),
            cbind(fromX, fromY),
            copyMapNV = copyMap
        )
        return(processPointMapResult(map, result))
    }
}
