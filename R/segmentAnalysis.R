# SPDX-FileCopyrightText: 2019 Fani Kostourou
# SPDX-FileCopyrightText: 2019-2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

segmentAnalysis <- function(segmentGraph,
                            radii,
                            radiusStepType,
                            analysisStepType,
                            weightWithColumn = NULL,
                            includeChoice = FALSE,
                            tulipBins = NA,
                            verbose = FALSE,
                            selOnly = FALSE,
                            progress = FALSE) {
  if (!(analysisStepType %in% Traversal)) {
    stop("Unknown segment analysis type: ", analysisStepType)
  }
  if (!(radiusStepType %in% Traversal)) {
    stop("Unknown radius type: ", radiusStepType)
  }

  numRadii <- vapply(radii, function(r) {
    if (r == "n") {
      return(-1L)
    } else {
      return(as.integer(r))
    }
  }, FUN.VALUE = 1L)

  return(Rcpp_runSegmentAnalysis(
    segmentGraph@ptr,
    numRadii,
    radiusStepType,
    analysisStepType,
    weightWithColumn,
    includeChoice,
    tulipBins,
    verbose,
    selOnly,
    progress
  ))
}

segmentAnalysisSf <- function(lineStringMap,
                              radii,
                              radiusStepType,
                              analysisStepType,
                              weightWithColumn = NULL,
                              includeChoice = FALSE,
                              tulipBins = NA,
                              keepAttributes = NULL,
                              throughAxial = TRUE,
                              verbose = FALSE,
                              selOnly = FALSE,
                              progress = FALSE) {
  weightByIdx <- NULL
  if (weightWithColumn != "" && !is.null(weightWithColumn)) {
    weightByIdx <- which(names(lineStringMap) == weightWithColumn)[[1L]]
  }

  keepAttributesFinal <- NULL
  if (!is.null(keepAttributes)) {
    keepAttributesFinal <- keepAttributes
  }
  if (!is.null(weightByIdx) && !(weightByIdx %in% keepAttributesFinal)) {
    if (is.null(keepAttributesFinal)) {
      keepAttributesFinal <- weightByIdx
    } else {
      keepAttributesFinal <- c(keepAttributesFinal, weightByIdx)
    }
  }
  segmentGraph <- sfToSegmentShapeGraph(
    lineStringMap,
    keepAttributesFinal,
    throughAxial = throughAxial
  )

  expectdAttrName <- NULL
  if (!is.null(weightByIdx)) {
    if (throughAxial) {
      expectdAttrName <- Rcpp_getAxialToSegmentExpectedColName(
        Rcpp_getSfShapeMapExpectedColName(lineStringMap, weightByIdx)
      )
    } else {
      expectdAttrName <-
        Rcpp_getSfShapeMapExpectedColName(lineStringMap, weightByIdx)
    }
  }

  segmentAnalysis(
    segmentGraph,
    radii,
    radiusStepType,
    analysisStepType,
    expectdAttrName,
    includeChoice,
    tulipBins,
    verbose,
    selOnly,
    progress
  )

  return(segmentShapeGraphToSf(segmentGraph))
}
