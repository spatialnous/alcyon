# SPDX-FileCopyrightText: 2019-2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

#' VGA Local Analysis algorithms.
#'
#' Different algorithms for calculating the VGA Local metrics (Control,
#' Controllability, Clustering Coefficient).
#' \itemize{
#'   \item{VGALocalAlgorithm$None}
#'   \item{VGALocalAlgorithm$Standard}
#'   \item{VGALocalAlgorithm$AdjacencyMatrix}
#' }
#'
#' @returns A list of numbers representing each algorithm
#' @examples
#' VGALocalAlgorithm$Angular
#' VGALocalAlgorithm$Topological
#' VGALocalAlgorithm$Metric
#' @export
VGALocalAlgorithm <- list(
    None = 0L,
    Standard = 1L,
    AdjacencyMatrix = 2L
)

#' Visibility Graph Analysis - Visual local metrics
#'
#' Runs Visibility Graph Analysis to get visual local metrics
#'
#' @param pointMap A PointMap
#' @param nthreads Optional. Number of threads to use (defaults to 1)
#' @param algorithm Optional. The algorithm to use. See ?VGALocalAlgorithm
#' @param copyMap Optional. Copy the internal sala map
#' @param gatesOnly Optional. Only keep the values at specific gates
#' @param progress Optional. Enable progress display
#' @returns A new PointMap with the results included
#' @eval c("@examples",
#' rxLoadSimpleLinesAsPointMap(),
#' "vgaVisualLocal(pointMap, FALSE)")
#' @export
vgaVisualLocal <- function(pointMap,
                           nthreads = 1L,
                           algorithm = VGALocalAlgorithm$Standard,
                           copyMap = TRUE,
                           gatesOnly = FALSE,
                           progress = FALSE) {
    result <- Rcpp_VGA_visualLocal(
        attr(pointMap, "sala_map"),
        gatesOnly,
        nthreadsNV = nthreads,
        algorithmNV = algorithm,
        copyMapNV = copyMap,
        progressNV = progress
    )
    if (result$cancelled) {
        stop("Analysis cancelled")
    }
    return(processPointMapResult(pointMap, result))
}
