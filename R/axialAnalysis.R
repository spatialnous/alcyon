# SPDX-FileCopyrightText: 2019 Fani Kostourou
# SPDX-FileCopyrightText: 2019-2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

axialAnalysis <- function(shapeGraph,
                          radii,
                          weightByAttribute = "",
                          includeChoice = FALSE,
                          includeIntermediateMetrics = FALSE,
                          keepGraph = FALSE,
                          verbose = FALSE) {
  numRadii <- vapply(radii, function(r) {
    if (r == "n") {
      return(-1L)
    } else {
      return(as.integer(r))
    }
  }, FUN.VALUE = 1L)

  return(Rcpp_runAxialAnalysis(
    shapeGraph@ptr,
    numRadii,
    weightByAttribute,
    includeChoice,
    includeIntermediateMetrics
  ))
}

#' Axial analysis - local metrics
#'
#' Runs axial analysis to get the local metrics Control and Controllability
#'
#' @param shapeGraph An Axial ShapeGraph
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
    verbose = FALSE) {
  return(Rcpp_runAxialLocalAnalysis(
    shapeGraph@ptr,
    verbose
  ))
}
