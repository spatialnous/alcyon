# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

#' Create an All-line Map
#'
#' @param boundsMap The boundary ShapeMap to create the all-line map in
#' @param seedX X coordinate of the seed (the point that initiates the process)
#' @param seedY Y coordinate of the seed (the point that initiates the process)
#' @param verbose Optional. Show more information of the process.
#' @returns An All-line Axial ShapeGraph
#' @eval c("@examples",
#' rxLoadSimpleLinesAsShapeMap(),
#' "makeAllLineMap(",
#' "  shapeMap,",
#' "  seedX = 3.01,",
#' "  seedY = 6.7",
#' ")")
#' @export
makeAllLineMap <- function(boundsMap,
                           seedX,
                           seedY,
                           verbose = FALSE) {
  allLineMap <- new("AllLineShapeGraph")
  allLineMap@ptr <- Rcpp_makeAllLineMap(
    boundsMap@ptr,
    seedX = seedX,
    seedY = seedY
  )
  return(allLineMap)
}


#' Reduce an All-line Map to two types of fewest-line maps
#'
#' @param allLineMap An AllLineShapeGraph
#' @returns A list with two fewest-line axial ShapeGraphs
#' @eval c("@examples",
#' rxLoadSimpleLinesAsShapeMap(),
#' "allLineMap <- makeAllLineMap(",
#' "  shapeMap,",
#' "  seedX = 3.01,",
#' "  seedY = 6.7",
#' ")",
#' "reduceToFewest(allLineMap)")
#' @export
reduceToFewest <- function(allLineMap) {
  result <- list()
  fewestMaps <- Rcpp_extractFewestLineMaps(allLineMap@ptr)
  fewestSubsets <- new("AxialShapeGraph")
  fewestSubsets@ptr <- fewestMaps[["Fewest-Line Map (Subsets)"]]
  fewestMinimal <- new("AxialShapeGraph")
  fewestMinimal@ptr <- fewestMaps[["Fewest-Line Map (Minimal)"]]
  result[["Fewest-Line Map (Subsets)"]] <- fewestSubsets
  result[["Fewest-Line Map (Minimal)"]] <- fewestMinimal
  return(result)
}
