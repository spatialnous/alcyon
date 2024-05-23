# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

#' Agent Analysis
#'
#' Runs Agent Analysis on the given PointMap
#'
#' @param pointMap A PointMap, used as an exosomatic visual map for agents to
#' take exploratory information
#' @param timesteps Number of total system timesteps.
#' @param releaseRate Agent release rate (likelihood of release per timestep).
#' @param agentLifeTimesteps Agent total lifetime (in timesteps)
#' @param agentFov Agent field-of-view (out of 32 bins = 360).
#' @param agentStepsToDecision Agent steps before turn decision.
#' @param agentLookMode The agent look mode. See \link{AgentLookMode}
#' @param originX Agent starting points (x coordinates).
#' @param originY Agent starting point (y coordinates).
#' @param locationSeed Agents to start at random locations with specific seed (0
#' to 10). Default is 0.
#' @param numberOfTrails Record trails for this amount of agents (set to 0 to
#' record all, with max possible currently = 50).
#' @param getGateCounts Get values at gates
#' @param verbose Optional. Show more information of the process.
#' @returns Returns a list with:
#' \itemize{
#'   \item{newAttributes: The new attributes that were created during the
#'   process}
#'   \item{trailMap: A ShapeMap with trails if numberOfTrails was set over 0}
#' }
#' @eval c("@examples",
#' rxLoadSimpleLinesAsPointMap(),
#' "agentAnalysis(",
#' "  pointMap,",
#' "  timesteps = 3000L,",
#' "  releaseRate = 0.1,",
#' "  agentStepsToDecision = 3L,",
#' "  agentFov = 11L,",
#' "  agentLife = 1000L,",
#' "  agentLookMode = AgentLookMode$Standard,",
#' "  originX = NA,",
#' "  originY = NA,",
#' "  locationSeed = 1L,",
#' "  numberOfTrails = 50L,",
#' "  getGateCounts = FALSE,",
#' "  verbose = FALSE",
#' ")")
#' @importFrom utils hasName
#' @export
agentAnalysis <- function(pointMap,
                          timesteps,
                          releaseRate,
                          agentLifeTimesteps,
                          agentFov,
                          agentStepsToDecision,
                          agentLookMode,
                          originX = NA,
                          originY = NA,
                          locationSeed = 0L,
                          numberOfTrails = NA,
                          getGateCounts = FALSE,
                          verbose = FALSE) {
  if (!(agentLookMode %in% AgentLookMode)) {
    stop("Unknown agent look mode: ", agentLookMode)
  }
  agentAnalysis <- Rcpp_agentAnalysis(
    pointMapPtr = pointMap@ptr,
    systemTimesteps = timesteps,
    releaseRate = releaseRate,
    agentLifeTimesteps = agentLifeTimesteps,
    agentFov = agentFov,
    agentStepsToDecision = agentStepsToDecision,
    agentLookMode = agentLookMode,
    agentReleaseLocations = cbind(originX, originY),
    randomReleaseLocationSeed = locationSeed,
    recordTrailForAgents = numberOfTrails,
    getGateCounts = getGateCounts,
    verbose = verbose
  )
  if (hasName(agentAnalysis, "trailMap")) {
    agentAnalysis$trailMap <- new("ShapeMap", ptr = agentAnalysis$trailMap)
  }
  return(agentAnalysis)
}
