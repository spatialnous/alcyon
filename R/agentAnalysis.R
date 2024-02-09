# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

agentAnalysis <- function(graphFileIn,
                          graphFileOut,
                          lookMode,
                          timesteps,
                          releaseRate,
                          agentFov,
                          agentSteps,
                          agentLife,
                          originX = NA,
                          originY = NA,
                          locationSeed = 0L,
                          numberOfTrails = NA,
                          outputType = "graph",
                          cliPath = getDefaultCLILocation(),
                          verbose = FALSE) {
  if (is.na(graphFileOut)) graphFileOut <- graphFileIn
  if (!(lookMode %in% c(
    "standard", "los-length", "occ-length", "occ-any",
    "occ-group-45", "occ-group-60", "occ-furthest",
    "bin-far-dist", "bin-angle", "bin-far-dist-angle",
    "bin-memory"
  ))) {
    stop("Unknown agent look mode: ", lookMode)
  }
  if (!(outputType %in% c("graph", "gatecounts", "trails"))) {
    stop("Unknown output type: ", outputType)
  }
  params <- c(
    "-f", formatForCLI(graphFileIn),
    "-o", formatForCLI(graphFileOut),
    "-m", "AGENTS",
    "-am", lookMode,
    "-ats", timesteps,
    "-arr", releaseRate,
    "-afov", agentFov,
    "-asteps", agentSteps,
    "-alife", agentLife,
    "-alocseed", locationSeed,
    "-ot", outputType
  )
  if (!is.na(numberOfTrails)) params <- c(params, "-atrails", numberOfTrails)
  if (!is.na(originX)) {
    tmpPtz <- makeTempPointFile(originX, originY)
    params <- c(params, "-alocfile", tmpPtz)
  }
  depthmapXcli(params, cliPath, verbose)
}
