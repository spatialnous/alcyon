# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

allFewestLineMap <- function(graphFileIn,
                             graphFileOut,
                             seedX,
                             seedY,
                             calculateFewest = TRUE,
                             cliPath = getDefaultCLILocation(),
                             verbose = FALSE) {
  if (is.na(graphFileOut)) graphFileOut <- graphFileIn
  params <- c(
    "-f", formatForCLI(graphFileIn),
    "-o", formatForCLI(graphFileOut),
    "-m", "AXIAL",
    "-xl", paste0(seedX, ",", seedY)
  )
  if (calculateFewest) params <- c(params, "-xf")
  depthmapXcli(params, cliPath, verbose)
}
