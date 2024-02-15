# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

createGrid <- function(boundsMap,
                       gridSize,
                       cliPath = getDefaultCLILocation(),
                       verbose = FALSE) {
  if (is.na(graphFileOut)) graphFileOut <- graphFileIn
  params <- c(
    "-f", formatForCLI(graphFileIn),
    "-o", formatForCLI(graphFileOut),
    "-m", "VISPREP",
    "-pg", gridSize
  )
  depthmapXcli(params, cliPath, verbose)
}

fillGrid <- function(graphFileIn,
                     graphFileOut,
                     fillX,
                     fillY,
                     cliPath = getDefaultCLILocation(),
                     verbose = FALSE) {
  if (is.na(graphFileOut)) graphFileOut <- graphFileIn
  tmpPtz <- tempfile(fileext = ".tsv")
  dt <- data.frame(x = fillX, y = fillY)
  write.table(dt, tmpPtz, row.names = FALSE, quote = FALSE, sep = "\t")

  params <- c(
    "-f", formatForCLI(graphFileIn),
    "-o", formatForCLI(graphFileOut),
    "-m", "VISPREP",
    "-pf", tmpPtz
  )

  depthmapXcli(params, cliPath, verbose)
  file.remove(tmpPtz)
}

makeVGAGraph <- function(graphFileIn,
                         graphFileOut = NA,
                         maxVisibility = NA,
                         boundaryGraph = FALSE,
                         cliPath = getDefaultCLILocation(),
                         verbose = FALSE) {
  if (is.na(graphFileOut)) graphFileOut <- graphFileIn
  params <- c(
    "-f", formatForCLI(graphFileIn),
    "-o", formatForCLI(graphFileOut),
    "-m", "VISPREP",
    "-pm"
  )
  if (!is.na(maxVisibility)) params <- c(params, "-pr", maxVisibility)
  if (boundaryGraph) params <- c(params, "-pb")

  depthmapXcli(params, cliPath, verbose)
}

unmakeVGAGraph <- function(graphFileIn,
                           graphFileOut = NA,
                           removeLinks = FALSE,
                           cliPath = getDefaultCLILocation(),
                           verbose = FALSE) {
  if (is.na(graphFileOut)) graphFileOut <- graphFileIn
  params <- c(
    "-f", formatForCLI(graphFileIn),
    "-o", formatForCLI(graphFileOut),
    "-m", "VISPREP",
    "-pu"
  )

  if (removeLinks) params <- c(params, "-pl")

  depthmapXcli(params, cliPath, verbose)
}
