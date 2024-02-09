# SPDX-FileCopyrightText: 2019 Fani Kostourou
# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

segmentAnalysis <- function(graphFileIn,
                            graphFileOut,
                            analysisType,
                            radii,
                            radiusType,
                            tulipBins = NA,
                            weightWithColumn = NA,
                            includeChoice = FALSE,
                            cliPath = getDefaultCLILocation(),
                            verbose = FALSE) {
  if (is.na(graphFileOut)) graphFileOut <- graphFileIn
  if (!(analysisType %in% c("tulip", "metric", "angular", "topological"))) {
    stop("Unknown segment analysis type: ", analysisType)
  }
  if (!(radiusType %in% c("steps", "metric", "angular"))) {
    stop("Unknown radius type: ", radiusType)
  }
  params <- c(
    "-f", formatForCLI(graphFileIn),
    "-o", formatForCLI(graphFileOut),
    "-m", "SEGMENT",
    "-st", analysisType,
    "-sr", paste(radii, collapse = ","),
    "-srt", radiusType
  )
  if (includeChoice) params <- c(params, "-sic")
  if (!is.na(tulipBins)) params <- c(params, "-stb", tulipBins)
  if (!is.na(weightWithColumn)) {
    params <- c(
      params, "-swa",
      formatForCLI(weightWithColumn)
    )
  }
  depthmapXcli(params, cliPath, verbose)
}
