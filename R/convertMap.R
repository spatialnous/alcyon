# SPDX-FileCopyrightText: 2019-2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

convertMap <- function(graphFileIn,
                       graphFileOut = NA,
                       newMapType = "axial",
                       newMapName = generateRandomCapString(10L),
                       removeInputMap = FALSE,
                       copyAttributes = FALSE,
                       stubLengthToRemove = NA,
                       cliPath = getDefaultCLILocation(),
                       verbose = FALSE) {
  if (is.na(graphFileOut)) graphFileOut <- graphFileIn
  if (!(newMapType %in% c("drawing", "axial", "segment", "data", "convex"))) {
    stop("Unknown map type: ", newMapType)
  }
  params <- c(
    "-f", formatForCLI(graphFileIn),
    "-o", formatForCLI(graphFileOut),
    "-m", "MAPCONVERT",
    "-co", newMapType,
    "-con", formatForCLI(newMapName)
  )
  if (removeInputMap) params <- c(params, "-cir")
  if (copyAttributes) params <- c(params, "-coc")
  if (!is.na(stubLengthToRemove)) {
    params <- c(
      params, "-crsl",
      stubLengthToRemove
    )
  }
  depthmapXcli(params, cliPath, verbose)
}
