# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

# The values here should be kept the same as the ones in AgentLookMode.h

# The values here should be kept the same order as the ones in TraversalType.h

#' Agent look modes.
#'
#' These are meant to be used to indicate what kind of look function
#' the agents use to look around and decide where to go next. Possible values:
#' \itemize{
#'   \item{AgentLookMode$None}
#'   \item{AgentLookMode$Standard}
#'   \item{AgentLookMode$LineOfSightLength}
#'   \item{AgentLookMode$OcclusionLength}
#'   \item{AgentLookMode$OcclusionAny}
#'   \item{AgentLookMode$OcclusionGroup45 (Occlusion group bins - 45 degrees)}
#'   \item{AgentLookMode$OcclusionGroup60 (Occlusion group bins - 60 degrees)}
#'   \item{AgentLookMode$OcclusionFurthest (Furthest occlusion per bin)}
#'   \item{AgentLookMode$BinFarDistance (Per bin far distance weighted)}
#'   \item{AgentLookMode$BinAngle (Per bin angle weighted)}
#'   \item{AgentLookMode$BinFarDistanceAngle (Per bin far-distance and angle
#'   weighted)}
#'   \item{AgentLookMode$BinMemory (Per bin memory)}
#' }
#'
#' @returns A list of numbers representing each agent look mode
#' @examples
#' AgentLookMode$Standard
#' AgentLookMode$LineOfSightLength
#' AgentLookMode$OcclusionAny
#' @export
AgentLookMode <- list(
    None = 0L,
    Standard = 1L,
    LineOfSightLength = 2L,
    OcclusionLength = 3L,
    OcclusionAny = 4L,
    OcclusionGroup45 = 5L,
    OcclusionGroup60 = 6L,
    OcclusionFurthest = 7L,
    BinFarDistance = 8L,
    BinAngle = 9L,
    BinFarDistanceAngle = 10L,
    BinMemory = 11L
)
