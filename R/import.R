# SPDX-FileCopyrightText: 2019 Fani Kostourou
# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

import <- function(filesToImport,
                   graphFileOut,
                   cliPath = getDefaultCLILocation(),
                   verbose = FALSE) {
  params <- c(
    "-f", formatForCLI(filesToImport[[1L]]),
    "-o", formatForCLI(graphFileOut),
    "-m", "IMPORT"
  )
  for (fileToImport in tail(filesToImport, length(filesToImport) - 1L)) {
    params <- c(params, "-if", fileToImport)
  }
  depthmapXcli(params, cliPath, verbose)
}

importLines <- function(linesIn,
                        graphFileOut,
                        cliPath = getDefaultCLILocation(),
                        verbose = FALSE) {
  if (class(linesIn)[[1L]] != "sf") {
    stop(
      "Lines in can only be of type sf, not: ",
      class(linesIn)[[1L]]
    )
  }

  startCoords <- st_coordinates(st_line_sample(linesIn, sample = 0L))
  endCoords <- st_coordinates(st_line_sample(linesIn, sample = 1L))

  linesDF <- st_drop_geometry(linesIn)
  linesDF$x1 <- startCoords[, "X"]
  linesDF$y1 <- startCoords[, "Y"]
  linesDF$x2 <- endCoords[, "X"]
  linesDF$y2 <- endCoords[, "Y"]

  tmpGraph <- tempfile(fileext = ".tsv")
  write.table(linesDF, tmpGraph, row.names = FALSE, quote = FALSE, sep = "\t")

  alcyon::import(tmpGraph, graphFileOut)
}
