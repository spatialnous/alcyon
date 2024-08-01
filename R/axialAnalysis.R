# SPDX-FileCopyrightText: 2019 Fani Kostourou
# SPDX-FileCopyrightText: 2019-2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

axialAnalysis <- function(shapeGraph,
                          radii,
                          weightByAttribute = NULL,
                          includeChoice = FALSE,
                          includeIntermediateMetrics = FALSE,
                          copyMap = TRUE,
                          keepGraph = FALSE,
                          verbose = FALSE) {
    numRadii <- vapply(radii, function(r) {
        if (r == "n") {
            return(-1L)
        } else {
            return(as.integer(r))
        }
    }, FUN.VALUE = 1L)

    result <- Rcpp_runAxialAnalysis(
        attr(shapeGraph, "sala_map"),
        numRadii,
        weightByAttribute,
        includeChoice,
        includeIntermediateMetrics,
        copyMapNV = copyMap
    )

    return(processShapeMapResult(shapeGraph, result))
}

#' Axial analysis - local metrics
#'
#' Runs axial analysis to get the local metrics Control and Controllability
#'
#' @param shapeGraph An Axial ShapeGraph
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
#' rxLoadSmallAxialLines(),
#' "axialAnalysisLocal(shapeGraph)")
#' @export
axialAnalysisLocal <- function(
        shapeGraph,
        copyMap = TRUE,
        verbose = FALSE) {
    result <- Rcpp_runAxialLocalAnalysis(
        attr(shapeGraph, "sala_map"),
        copyMapNV = copyMap,
        verbose
    )
    return(processShapeMapResult(shapeGraph, result))
}
