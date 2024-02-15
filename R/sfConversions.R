# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

sfToShapeMap <- function(lineStringMap,
                         keepAttributes = NULL) {
  return(Rcpp_toShapeMap(lineStringMap, keepAttributes))
}

sfToAxialShapeGraph <- function(lineStringMap,
                                keepAttributes = NULL) {
  shapeMap <- Rcpp_toShapeMap(lineStringMap, keepAttributes)
  return(Rcpp_toAxialShapeGraph(shapeMap))
}

sfToSegmentShapeGraph <- function(lineStringMap,
                                  keepAttributes = NULL,
                                  throughAxial = FALSE) {
  shapeMap <- Rcpp_toShapeMap(lineStringMap, keepAttributes)
  if (throughAxial) {
    axialShapeGraph <- Rcpp_toAxialShapeGraph(shapeMap)
    return(Rcpp_axialToSegment(axialShapeGraph))
  }
  return(Rcpp_shapeMapToSegment(shapeMap))
}

shapeMapTolineStringSf <- function(shapeMap) {
  coords <- Rcpp_ShapeMap_getShapesAsLineCoords(shapeMap)
  sfGeom <- st_sfc(lapply(seq_len(nrow(coords)), function(rowIdx) {
    sf::st_linestring(matrix(coords[rowIdx, ], ncol = 2L, byrow = TRUE),
      dim = "XY"
    )
  }))

  attrNames <- Rcpp_ShapeMap_getAttributeNames(shapeMap)
  result <- st_sf(Rcpp_ShapeMap_getAttributeData(shapeMap, attrNames),
                  geometry = sfGeom)
  return(result[c(attrNames, "geometry")])
}

shapeMapToPolygongSf <- function(shapeMap) {
  coords <- Rcpp_ShapeMap_getShapesAsPolygonCoords(shapeMap)
  sfGeom <- st_sfc(lapply(coords, function(polyCoords) {
    sf::st_polygon(list(polyCoords), dim = "XY")
  }))

  attrNames <- Rcpp_ShapeMap_getAttributeNames(shapeMap)
  result <- st_sf(Rcpp_ShapeMap_getAttributeData(shapeMap, attrNames),
                  geometry = sfGeom)
  return(result[c(attrNames, "geometry")])
}

axialShapeGraphToSf <- function(shapeGraph) {
  return(list(
    map = shapeMapTolineStringSf(shapeGraph),
    graph = Rcpp_ShapeGraph_getAxialConnections(shapeGraph)
  ))
}

segmentShapeGraphToSf <- function(shapeGraph) {
  return(list(
    map = shapeMapTolineStringSf(shapeGraph),
    graph = Rcpp_ShapeGraph_getSegmentConnections(shapeGraph)
  ))
}
