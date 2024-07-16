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
  allLineMapPtr <- Rcpp_makeAllLineMap(
    attr(boundsMap, "sala_map"),
    seedX = seedX,
    seedY = seedY
  )
  newMap <- processPtrAsNewLineMap(allLineMapPtr$allLineMap,
                                   c("AllLineShapeGraph",
                                     "AxialShapeGraph",
                                     "ShapeMap"))
  attr(newMap, "all_line_data") <- allLineMapPtr$mapData
  return(newMap)
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
  fewestMaps <- Rcpp_extractFewestLineMaps(attr(allLineMap, "sala_map"),
                                           attr(allLineMap, "all_line_data"))
  result[["Fewest-Line Map (Subsets)"]] <-
    processPtrAsNewLineMap(fewestMaps[["Fewest-Line Map (Subsets)"]],
                           c("AxialShapeGraph", "ShapeMap"))
  result[["Fewest-Line Map (Minimal)"]] <-
    processPtrAsNewLineMap(fewestMaps[["Fewest-Line Map (Minimal)"]],
                           c("AxialShapeGraph", "ShapeMap"))
  return(result)
}
