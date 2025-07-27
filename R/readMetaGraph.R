# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

#' Read MetaGraph
#'
#' Reads a metagraph into a bunch of ShapeMaps/ShapeGraphs/LatticeMaps
#'
#' @param fileName The metagraph file
#' @returns A list of ShapeMaps, ShapeGraphs and LatticeMaps
#' @examples
#' fileName <- system.file(
#'     "extdata", "testdata", "barnsbury", "barnsburySmall.graph",
#'     package = "alcyon"
#' )
#' readMetaGraph(fileName)
#' @export
readMetaGraph <- function(fileName) {
    result <- list(
        shapeMaps = list(),
        axialShapeGraphs = list(),
        alllineShapeGraphs = list(),
        segmentShapeGraphs = list(),
        latticeMaps = list()
    )
    mgraphData <- Rcpp_MetaGraph_read(fileName)
    if (length(mgraphData$shapeMaps) > 0L) {
        result$shapeMaps <- lapply(mgraphData$shapeMaps, function(mapData) {
            return(processPtrAsNewLineMap(mapData$ptr, "ShapeMap"))
        })
    }
    if (length(mgraphData$latticeMaps) > 0L) {
        for (mapData in mgraphData$latticeMaps) {
            result$latticeMaps <-
                c(
                    result$latticeMaps,
                    list(processPtrAsNewLatticeMap(mapData$ptr))
                )
        }
    }
    for (mapData in mgraphData$shapeGraphs) {
        switch(mapData$type, axial = {
            result$axialShapeGraphs <-
                c(
                    result$axialShapeGraphs,
                    list(processPtrAsNewLineMap(mapData$ptr, c(
                        "AxialShapeGraph",
                        "ShapeMap"
                    )))
                )
        },
        allline = {
            result$alllineShapeGraphs <-
                c(
                    result$alllineShapeGraphs,
                    list(processPtrAsNewLineMap(mapData$ptr, c(
                        "AllLineShapeGraph",
                        "AxialShapeGraph",
                        "ShapeMap"
                    )))
                )
        }, segment = {
            result$segmentShapeGraphs <-
                c(
                    result$segmentShapeGraphs,
                    list(processPtrAsNewLineMap(mapData$ptr, c(
                        "SegmentShapeGraph",
                        "ShapeMap"
                    )))
                )
        })
    }
    return(result)
}
