# SPDX-FileCopyrightText: 2019 Fani Kostourou
# SPDX-FileCopyrightText: 2019-2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

segmentAnalysis <- function(segmentGraph,
                            radii,
                            radiusStepType,
                            analysisStepType,
                            weightWithColumn = NULL,
                            includeChoice = FALSE,
                            tulipBins = 0L,
                            selOnly = FALSE,
                            copyMap = TRUE,
                            verbose = FALSE,
                            progress = FALSE) {
  if (!(analysisStepType %in% as.list(TraversalType))) {
    stop("Unknown segment analysis type: ", analysisStepType)
  }
  if (!(radiusStepType %in% as.list(TraversalType))) {
    stop("Unknown radius type: ", radiusStepType)
  }

  numRadii <- vapply(radii, function(r) {
    if (r == "n") {
      return(-1L)
    } else {
      return(as.integer(r))
    }
  }, FUN.VALUE = 1L)

  result <- Rcpp_runSegmentAnalysis(
    attr(segmentGraph, "sala_map"),
    numRadii,
    radiusStepType,
    analysisStepType,
    weightWithColumn,
    includeChoice,
    tulipBins,
    selOnlyNV = selOnly,
    copyMapNV = copyMap,
    verboseNV = verbose,
    progressNV = progress
  )
  return(processShapeMapResult(segmentGraph, result))
}
