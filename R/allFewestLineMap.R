# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

allFewestLineMap <- function(boundsMap,
                             seedX,
                             seedY,
                             calculateFewest = TRUE,
                             verbose = FALSE) {
  allLineMap <- new("AllLineShapeGraph")
  allLineMap@ptr <- Rcpp_makeAllLineMap(
    boundsMap@ptr,
    seedX = 3.01,
    seedY = 6.7
  )

  result <- list()
  result[["All Line Map"]] <- shapeMapTolineStringSf(allLineMap)

  if (calculateFewest) {
    fewestMaps <- Rcpp_extractFewestLineMaps(allLineMap@ptr)
    fewestSubsets <- new("AxialShapeGraph")
    fewestSubsets@ptr <- fewestMaps[["Fewest-Line Map (Subsets)"]]
    fewestMinimal <- new("AxialShapeGraph")
    fewestMinimal@ptr <- fewestMaps[["Fewest-Line Map (Minimal)"]]
    result[["Fewest-Line Map (Subsets)"]] <-
      shapeMapTolineStringSf(fewestSubsets)
    result[["Fewest-Line Map (Minimal)"]] <-
      shapeMapTolineStringSf(fewestMinimal)
  }
  return(result)
}
