# SPDX-FileCopyrightText: 2019 Fani Kostourou
# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

shapegraphToIGraph <- function(graphFile,
                               weightcolumn = NA) {
  ogr <- getShapeGraph(graphFile)
  linksunlinks <- getShapeGraphLinksUnlinks(graphFile)
  links <- linksunlinks[linksunlinks$link == 1L, ]
  links <- links[, c("refA", "refB")]
  unlinks <- linksunlinks[linksunlinks$link == 0L, ]
  unlinks <- unlinks[, c("refA", "refB")]
  connections <- alcyon::getShapeGraphConnections(graphFile)
  if (nrow(connections) == 0L) {
    edges <- links
  } else {
    edges <- connections
  }

  for (i in seq_len(nrow(edges))) {
    edges[i, ] <- sort(edges[i, c("refA", "refB")])
  }
  edges <- edges[!duplicated(edges), ]

  ogr$x <- as.data.frame(sf::st_centroid(ogr))[, 1L]
  ogr$y <- as.data.frame(sf::st_centroid(ogr))[, 2L]

  refA <- edges$refA
  refB <- edges$refB
  depthRef <- ogr$Depthmap_Ref
  ogr <- ogr[, c("Depthmap_Ref", names(ogr)[names(ogr) != "Depthmap_Ref"])]
  if (!is.na(weightcolumn)) {
    edges$weight <- ((ogr[[match(refA, depthRef), weightcolumn]]) +
                       (ogr[[match(refB, depthRef), weightcolumn]])) / 2.0
    graph <- graph.data.frame(
      edges,
      directed = FALSE,
      vertices = st_drop_geometry(ogr)
    )
    E(graph)$weight <- edges[["weight"]]
  } else {
    graph <- graph.data.frame(
      edges,
      directed = FALSE,
      vertices = st_drop_geometry(ogr)
    )
  }
  return(graph)
}
