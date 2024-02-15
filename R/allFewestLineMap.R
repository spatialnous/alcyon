# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

allFewestLineMap <- function(boundsMap,
                             seedX,
                             seedY,
                             calculateFewest = TRUE,
                             verbose = FALSE) {
  shapeGraph <- sfToShapeMap(
    boundsMap,
    keepAttributes = vector(mode = "integer")
  )

  allLineMap <- Rcpp_makeAllLineMap(
    shapeGraph,
    seedX = 3.01,
    seedY = 6.7
  )

  result <- list()
  result[["All Line Map"]] <- shapeMapTolineStringSf(allLineMap)

  if (calculateFewest) {
    fewestMaps <- Rcpp_extractFewestLineMaps(allLineMap)
    result[["Fewest-Line Map (Subsets)"]] <-
      shapeMapTolineStringSf(fewestMaps[["Fewest-Line Map (Subsets)"]])
    result[["Fewest-Line Map (Minimal)"]] <-
      shapeMapTolineStringSf(fewestMaps[["Fewest-Line Map (Minimal)"]])
  }
  return(result)
}
