# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

stepdepth <- function(graphFileIn,
                      graphFileOut,
                      depthType,
                      fromX,
                      fromY,
                      cliPath = getDefaultCLILocation(),
                      verbose = FALSE) {
  if (!(depthType %in% c("metric", "angular", "visual"))) {
    stop("Unknown depthType: ", depthType)
  }

  tmpPtz <- makeTempPointFile(fromX, fromY)

  params <- c(
    "-f", formatForCLI(graphFileIn),
    "-o", formatForCLI(graphFileOut),
    "-m", "STEPDEPTH",
    "-sdt", depthType,
    "-sdf", formatForCLI(tmpPtz)
  )
  depthmapXcli(params, cliPath, verbose)
  invisible(file.remove(tmpPtz))
}
