# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

sfToShapeMap <- function(lineStringMap,
                         keepAttributes = NULL) {
  shapeMap <- new("ShapeMap")
  shapeMap@ptr <- Rcpp_toShapeMap(lineStringMap, keepAttributes)
  return(shapeMap)
}

sfToAxialShapeGraph <- function(lineStringMap,
                                keepAttributes = NULL) {
  shapeGraph <- new("AxialShapeGraph")
  shapeMap <- Rcpp_toShapeMap(lineStringMap, keepAttributes)
  shapeGraph@ptr <- Rcpp_toAxialShapeGraph(shapeMap)
  return(shapeGraph)
}

sfToSegmentShapeGraph <- function(lineStringMap,
                                  keepAttributes = NULL,
                                  throughAxial = FALSE) {
  shapeGraph <- new("SegmentShapeGraph")
  shapeMap <- Rcpp_toShapeMap(lineStringMap, keepAttributes)
  if (throughAxial) {
    axialShapeGraph <- Rcpp_toAxialShapeGraph(shapeMap)
    shapeGraph@ptr <- Rcpp_axialToSegment(axialShapeGraph)
  } else {
    shapeGraph@ptr <- Rcpp_shapeMapToSegment(shapeMap)
  }
  return(shapeGraph)
}

shapeMapTolineStringSf <- function(shapeMap) {
  coords <- Rcpp_ShapeMap_getShapesAsLineCoords(shapeMap@ptr)
  sfGeom <- st_sfc(lapply(seq_len(nrow(coords)), function(rowIdx) {
    sf::st_linestring(matrix(coords[rowIdx, ], ncol = 2L, byrow = TRUE),
      dim = "XY"
    )
  }))

  attrNames <- Rcpp_ShapeMap_getAttributeNames(shapeMap@ptr)
  result <- st_sf(Rcpp_ShapeMap_getAttributeData(shapeMap@ptr, attrNames),
    geometry = sfGeom
  )
  return(result[c(attrNames, "geometry")])
}

shapeMapToPolygongSf <- function(shapeMap) {
  coords <- Rcpp_ShapeMap_getShapesAsPolygonCoords(shapeMap@ptr)
  sfGeom <- st_sfc(lapply(coords, function(polyCoords) {
    sf::st_polygon(list(polyCoords), dim = "XY")
  }))

  attrNames <- Rcpp_ShapeMap_getAttributeNames(shapeMap@ptr)
  result <- st_sf(Rcpp_ShapeMap_getAttributeData(shapeMap@ptr, attrNames),
    geometry = sfGeom
  )
  return(result[c(attrNames, "geometry")])
}

axialShapeGraphToSf <- function(shapeGraph) {
  return(list(
    map = shapeMapTolineStringSf(shapeGraph),
    graph = Rcpp_ShapeGraph_getAxialConnections(shapeGraph@ptr)
  ))
}

segmentShapeGraphToSf <- function(shapeGraph) {
  return(list(
    map = shapeMapTolineStringSf(shapeGraph),
    graph = Rcpp_ShapeGraph_getSegmentConnections(shapeGraph@ptr)
  ))
}

pointMapToSf <- function(pointMap) {
  coords <- Rcpp_PointMap_getFilledPoints(pointMap = pointMap@ptr)
  map <- SpatialPointsDataFrame(coords[, c(1L, 2L)], data = data.frame(coords))
  gridded(map) <- TRUE
  return(list(
    map = map,
    graph = NULL
  ))
}
