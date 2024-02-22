# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

agentAnalysis <- function(pointMap,
                          timesteps,
                          releaseRate,
                          agentStepsToDecision,
                          agentFov,
                          agentLife,
                          lookMode,
                          originX = NA,
                          originY = NA,
                          locationSeed = 0L,
                          numberOfTrails = NA,
                          getGateCounts = FALSE,
                          verbose = FALSE) {
  if (!(lookMode %in% AgentLook)) {
    stop("Unknown agent look mode: ", lookMode)
  }
  return(Rcpp_agentAnalysis(
    pointMapPtr = pointMap@ptr,
    systemTimesteps = timesteps,
    releaseRate = releaseRate,
    agentStepsToDecision = agentStepsToDecision,
    agentFov = agentFov,
    agentLife = agentLife,
    agentLook = lookMode,
    agentReleaseLocations = cbind(originX, originY),
    randomReleaseLocationSeed = locationSeed,
    recordTrailForAgents = numberOfTrails,
    getGateCounts = getGateCounts,
    verbose = verbose
  ))
}
