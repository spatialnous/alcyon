# SPDX-FileCopyrightText: 2019 Fani Kostourou
# SPDX-FileCopyrightText: 2019-2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

#' Conversion of shapegraph to igraph
#'
#' Creates igraph based on the connections and the x,y coordinates of the
#' centroids of shapes in a shapegraph (axial,segment, convex). Specify
#' weightColumn to assign weight to graph edges.
#'
#' If weightColumn is provided, edge connections weight is calculated by taking
#' the average of the variable of the connected nodes.
#'
#' @param shapeGraph A ShapeGraph
#' @param weightColumn Optional.The variable used to assign weight to graph
#' edges
#' @importFrom igraph graph.data.frame E E<-
#' @importFrom sf st_drop_geometry
#' @returns Returns graph.data.frame.
shapegraphToIGraph <- function(shapeGraph,
                               weightColumn = NA) {
  ogr <- shapeGraph
  linksunlinks <- links(shapeGraph)
  links <- linksunlinks[linksunlinks$link == 1L, ]
  links <- links[, c("refA", "refB")]
  unlinks <- linksunlinks[linksunlinks$link == 0L, ]
  unlinks <- unlinks[, c("refA", "refB")]
  connections <- connections(shapeGraph)
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
  if (!is.na(weightColumn)) {
    edges$weight <- ((ogr[[match(refA, depthRef), weightColumn]])
                     + (ogr[[match(refB, depthRef), weightColumn]])) / 2.0
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
