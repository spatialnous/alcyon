# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

isovist <- function(boundsMap,
                    x,
                    y,
                    angle = NA,
                    viewAngle = NA,
                    verbose = FALSE) {
  shapeGraph <- sfToShapeMap(
    boundsMap,
    keepAttributes = vector(mode = "integer")
  )

  isovists <- Rcpp_makeIsovists(
    shapeGraph,
    cbind(x, y),
    angle,
    viewAngle,
    FALSE
  )

  polygonCoords <- Rcpp_ShapeMap_getShapesAsPolygonCoords(isovists)

  sfGeom <- st_sfc(lapply(polygonCoords, function(polyCoords) {
    st_polygon(list(polyCoords), dim = "XY")
  }))

  attrNames <- Rcpp_ShapeMap_getAttributeNames(isovists)
  result <- st_sf(
    Rcpp_ShapeMap_getAttributeData(isovists, attrNames),
    geometry = sfGeom
  )

  return(result)
}

isovist2pts <- function(boundsMap,
                        x,
                        y,
                        toX,
                        toY,
                        viewAngle,
                        verbose = FALSE) {
  angles <- 180.0 * atan2(toY - y, toX - x) / pi
  angles <- ifelse(angles < 0.0, 360.0 + angles, angles)
  isovist(boundsMap, x, y, angles, viewAngle, verbose)
}
