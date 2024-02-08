# Copyright 2019 Fani Kostourou
# Copyright 2019 Petros Koutsolampros
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <https://www.gnu.org/licenses/>.

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
