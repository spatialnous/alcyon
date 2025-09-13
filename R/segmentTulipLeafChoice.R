# SPDX-FileCopyrightText: 2019 Fani Kostourou
# SPDX-FileCopyrightText: 2019-2025 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only


#' Segment Tulip Leaf Choice
#'
#' This is the legacy calculation of choice where backwards traversal
#' was only started from "leaf" nodes i.e. nodes where the forwards
#' traversal originally found a dead end (from actual dead ends, to
#' dead ends because the next line has been covered through a different
#' path)
#'
#' @param map A Segment ShapeGraph
#' @param radii A list of radii
#' @param radiusTraversalType The traversal type to keep track of whether the
#' analysis is within the each radius limit. See \link{TraversalType}
#' @param weightByAttribute The attribute to weigh the analysis with
#' @param quantizationWidth Set this to use chunks of this width instead of
#' continuous values for the cost of traversal. This is equivalent to the "tulip
#' bins" for depthmapX's tulip analysis (1024 tulip bins = pi/1024
#' quantizationWidth). Only works for Segment ShapeGraphs
#' @param copyMap Optional. Copy the internal sala map
#' @param verbose Optional. Show more information of the process.
#' @param progress Optional. Enable progress display
#'
#' @returns A new map with the results included
#' @eval c("@examples",
#' "# LatticeMap analysis (VGA)",
#' rxLoadSimpleLinesAsLatticeMap(),
#' "allToAllTraverse(latticeMap,",
#' "  traversalType = TraversalType$Angular,",
#' "  radii = -1L,",
#' "  radiusTraversalType = TraversalType$None",
#' ")",
#' "",
#' "# Axial analysis",
#' rxLoadSmallAxialLines(),
#' "allToAllTraverse(",
#' "  shapeGraph,",
#' "  traversalType = TraversalType$Topological,",
#' "  radii = c(\"n\", \"3\"),",
#' "  includeBetweenness = TRUE",
#' ")",
#' "",
#' "# Segment analysis",
#' rxLoadSmallSegmentLines(),
#' "segmentTulipLeafChoice(",
#' "  shapeGraph,",
#' "  radii = c(\"n\", \"100\"),",
#' "  radiusTraversalType = TraversalType$Metric,",
#' "  weightByAttribute = \"Segment Length\",",
#' "  quantizationWidth = pi / 1024L,",
#' "  verbose = FALSE,",
#' "  progress = FALSE",
#' ")")
#' @export
segmentTulipLeafChoice <- function(map,
                                   radii,
                                   radiusTraversalType,
                                   weightByAttribute = NULL,
                                   quantizationWidth = NA,
                                   copyMap = TRUE,
                                   verbose = FALSE,
                                   progress = FALSE) {
    if (!(radiusTraversalType %in% as.list(TraversalType))) {
        stop("Unknown radius type: ", radiusTraversalType, call. = FALSE)
    }

    numRadii <- vapply(radii, function(r) {
        if (r == "n") {
            return(-1L)
        } else {
            return(as.integer(r))
        }
    }, FUN.VALUE = 1L)

    tulipBins <- as.integer(pi / quantizationWidth)

    result <- Rcpp_runSegmentTulipLeafChoice(
        attr(map, "sala_map"),
        numRadii,
        radiusTraversalType,
        weightByAttribute,
        tulipBins,
        selOnlyNV = FALSE,
        copyMapNV = copyMap,
        verboseNV = verbose,
        progressNV = progress
    )
    return(processShapeMapResult(map, result))
}
