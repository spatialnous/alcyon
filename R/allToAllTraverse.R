# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

#' All-to-all traversal
#'
#' Runs all-to-all traversal on a map with a graph. This is applicable to:
#' \itemize{
#'   \item{PointMaps (Visibility Graph Analysis)}
#'   \item{Axial ShapeGraphs (Axial analysis)}
#'   \item{Segment ShapeGraphs (Segment analysis)}
#' }
#'
#' @param map A PointMap, Axial ShapeGraph or Segment ShapeGraph
#' @param traversalType The traversal type. See \link{TraversalType}
#' @param radii A list of radii
#' @param radiusTraversalType The traversal type to keep track of whether the
#' analysis is within the each radius limit. See \link{TraversalType}
#' @param weightByAttribute The attribute to weigh the analysis with
#' @param includeBetweenness Set to TRUE to also calculate betweenness (known as
#' Choice in the Space Syntax domain)
#' @param quantizationWidth Set this to use chunks of this width instead of
#' continuous values for the cost of traversal. This is equivalent to the "tulip
#' bins" for depthmapX's tulip analysis (1024 tulip bins = pi/1024
#' quantizationWidth). Only works for Segment ShapeGraphs
#' @param gatesOnly Optional. Only calculate results at particular gate pixels.
#' Only works for PointMaps
#' @param nthreads Optional. Use more than one threads. 1 by default, set to 0
#' to use all available. Only available for PointMaps.
#' @param copyMap Optional. Copy the internal sala map
#' @param verbose Optional. Show more information of the process.
#' @param progress Optional. Enable progress display
#'
#' @returns A new map with the results included
#' @eval c("@examples",
#' "# Pointmap analysis (VGA)",
#' rxLoadSimpleLinesAsPointMap(),
#' "allToAllTraverse(pointMap,",
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
#' "allToAllTraverse(",
#' "  shapeGraph,",
#' "  radii = c(\"n\", \"100\"),",
#' "  radiusTraversalType = TraversalType$Metric,",
#' "  traversalType = TraversalType$Angular,",
#' "  weightByAttribute = \"Segment Length\",",
#' "  includeBetweenness = TRUE,",
#' "  quantizationWidth = pi / 1024L,",
#' "  verbose = FALSE,",
#' "  progress = FALSE",
#' ")")
#' @export
allToAllTraverse <- function(map,
                             traversalType,
                             radii,
                             radiusTraversalType,
                             weightByAttribute = NULL,
                             includeBetweenness = FALSE,
                             quantizationWidth = NA,
                             gatesOnly = FALSE,
                             nthreads = 1L,
                             copyMap = TRUE,
                             verbose = FALSE,
                             progress = FALSE) {
    if (!(traversalType %in% as.list(TraversalType))) {
        stop("Unknown traversalType type: ", traversalType, call. = FALSE)
    }
    if (!is.na(quantizationWidth) && !inherits(map, "SegmentShapeGraph")) {
        stop("quantizationWidth can only be used with Segment ShapeGraphs", call. = FALSE)
    }
    if (length(radii) < 1L) {
        stop("At least one radius is required", call. = FALSE)
    }

    if (!inherits(map, "PointMap") && nthreads != 1L) {
        stop("Setting the number of threads is only possible for PointMaps (VGA)",
             call. = FALSE)
    }

    if (inherits(map, "PointMap")) {
        return(allToAllTraversePointMap(
            map,
            traversalType,
            radii,
            radiusTraversalType,
            weightByAttribute,
            includeBetweenness,
            quantizationWidth,
            gatesOnly,
            nthreads,
            copyMap,
            verbose,
            progress
        ))
    } else if (inherits(map, "AxialShapeGraph")) {
        return(axialAnalysis(
            shapeGraph = map,
            radii = radii,
            weightByAttribute = weightByAttribute,
            includeChoice = includeBetweenness,
            includeIntermediateMetrics = FALSE,
            copyMap = copyMap,
            verbose = verbose
        ))
    } else if (inherits(map, "SegmentShapeGraph")) {
        tulipBins <- 0L
        if (traversalType == TraversalType$Angular &&
                !is.na(quantizationWidth)) {
            tulipBins <- as.integer(pi / quantizationWidth)
        }
        return(segmentAnalysis(
            segmentGraph = map,
            radii = radii,
            radiusStepType = radiusTraversalType,
            analysisStepType = traversalType,
            weightWithColumn = weightByAttribute,
            includeChoice = includeBetweenness,
            tulipBins = tulipBins,
            selOnly = FALSE,
            copyMap = copyMap,
            verbose = verbose,
            progress = progress
        ))
    } else {
        stop(
            "Can only run all-to-all traversal on Axial or Segment ",
            "ShapeGraphs and PointMaps", call. = FALSE
        )
    }
}

allToAllTraversePointMap <- function(map,
                                     traversalType,
                                     radii,
                                     radiusTraversalType,
                                     weightByAttribute = NULL,
                                     includeBetweenness = FALSE,
                                     quantizationWidth = NA,
                                     gatesOnly = FALSE,
                                     nthreads = 1L,
                                     copyMap = TRUE,
                                     verbose = FALSE,
                                     progress = FALSE) {
    if (traversalType == TraversalType$Metric) {
        analysisResult <- Rcpp_VGA_metric(
            attr(map, "sala_map"),
            radii[1L],
            gatesOnly,
            nthreadsNV = nthreads,
            copyMapNV = copyMap,
            progressNV = progress
        )
        if (analysisResult$cancelled) {
            stop("Analysis cancelled", call. = FALSE)
        }
        for (radius in radii[-1L]) {
            radiusAnalysisResult <- Rcpp_VGA_metric(
                attr(map, "sala_map"),
                radius,
                gatesOnly,
                nthreadsNV = nthreads,
                copyMapNV = FALSE,
                progressNV = progress
            )
            if ("cancelled" %in% radiusAnalysisResult) {
                stop("Analysis cancelled", call. = FALSE)
            }
            analysisResult$completed <- analysisResult$completed &
                radiusAnalysisResult$completed
            analysisResult$newAttributes <- c(
                analysisResult$newAttributes,
                radiusAnalysisResult$newAttributes
            )
        }
        return(processPointMapResult(map, analysisResult))
    } else if (traversalType == TraversalType$Topological) {
        analysisResult <- Rcpp_VGA_visualGlobal(
            attr(map, "sala_map"),
            radii[1L],
            gatesOnly,
            nthreadsNV = nthreads,
            copyMapNV = copyMap,
            progressNV = progress
        )
        for (radius in radii[-1L]) {
            radiusAnalysisResult <- Rcpp_VGA_visualGlobal(
                attr(map, "sala_map"),
                radius,
                gatesOnly,
                nthreadsNV = nthreads,
                copyMapNV = FALSE,
                progressNV = progress
            )
            analysisResult$completed <- analysisResult$completed &
                radiusAnalysisResult$completed
            analysisResult$newAttributes <- c(
                analysisResult$newAttributes,
                radiusAnalysisResult$newAttributes
            )
        }
        return(processPointMapResult(map, analysisResult))
    } else if (traversalType == TraversalType$Angular) {
        analysisResult <- Rcpp_VGA_angular(
            attr(map, "sala_map"),
            radii[1L],
            gatesOnly,
            nthreadsNV = nthreads,
            copyMapNV = copyMap,
            progressNV = progress
        )
        for (radius in radii[-1L]) {
            radiusAnalysisResult <- Rcpp_VGA_angular(
                attr(map, "sala_map"),
                radius,
                gatesOnly,
                nthreadsNV = nthreads,
                copyMapNV = FALSE,
                progressNV = progress
            )
            analysisResult$completed <- analysisResult$completed &
                radiusAnalysisResult$completed
            analysisResult$newAttributes <- c(
                analysisResult$newAttributes,
                radiusAnalysisResult$newAttributes
            )
        }
        return(processPointMapResult(map, analysisResult))
    }
}

#' Visibility Graph Analysis - Through Vision
#'
#' Runs Visibility Graph Analysis to get the Through Vision metric
#'
#' @param pointMap A PointMap
#' @param copyMap Optional. Copy the internal sala map
#' @returns A new PointMap with the results included
#' @eval c("@examples",
#' rxLoadSimpleLinesAsPointMap(),
#' "vgaThroughVision(pointMap)")
#' @export
vgaThroughVision <- function(pointMap,
                             copyMap = TRUE) {
    result <- Rcpp_VGA_throughVision(
        attr(pointMap, "sala_map"),
        copyMapNV = copyMap
    )
    return(processPointMapResult(pointMap, result))
}

#' Visibility Graph Analysis - isovist metrics
#'
#' Runs axial analysis to get the local metrics Control and Controllability
#'
#' @param pointMap A PointMap
#' @param boundaryMap A ShapeMap of lines
#' @param copyMap Optional. Copy the internal sala map
#' @returns A new PointMap with the results included
#' @eval c("@examples",
#' rxLoadSimpleLinesAsPointMap(),
#' "boundaryMap <- as(sfMap[, c()], \"ShapeMap\")",
#' "vgaIsovist(pointMap, boundaryMap)")
#' @export
vgaIsovist <- function(pointMap,
                       boundaryMap,
                       copyMap = TRUE) {
    result <- Rcpp_VGA_isovist(
        attr(pointMap, "sala_map"),
        attr(boundaryMap, "sala_map"),
        copyMapNV = copyMap
    )
    return(processPointMapResult(pointMap, result))
}
