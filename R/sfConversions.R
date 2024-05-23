# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

#' ShapeMap to sf Polygon map
#'
#' Convert a ShapeMap to an sf Polygon map
#'
#' @param shapeMap A ShapeMap
#' @returns A new sf Polygon map
#' @importFrom sf st_sf st_sfc
#' @eval c("@examples",
#' rxLoadSimpleLinesAsShapeMap(),
#' "isovistMap <- isovist(",
#' "  shapeMap,",
#' "  x = c(3.01, 1.3),",
#' "  y = c(6.70, 5.2),",
#' "  angle = 0.01,",
#' "  viewAngle = 3.14,",
#' "  FALSE",
#' ")",
#' "shapeMapToPolygonSf(isovistMap)")
#' @export
shapeMapToPolygonSf <- function(shapeMap) {
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
