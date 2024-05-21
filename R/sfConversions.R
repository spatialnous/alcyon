# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

#' Axial to Segment ShapeGraph
#'
#' Convert an Axial ShapeGraph to a Segment ShapeGraph
#'
#' @param axialShapeGraph An Axial ShapeGraph
#' @param stubRemoval Rremove stubs of axial lines shorter than this
#' percentage (for example provide 0.4 for 40\%)
#' @returns A new Segment ShapeGraph
#' @importFrom methods new
#' @export
axialToSegmentShapeGraph <- function(axialShapeGraph,
                                     stubRemoval = NULL) {
  shapeGraph <- new("SegmentShapeGraph")
  shapeGraph@ptr <- Rcpp_axialToSegment(
    axialShapeGraph@ptr,
    "Segment Map",
    TRUE,
    stubRemoval
  )
  return(shapeGraph)
}

#' ShapeMap to sf Polygon map
#'
#' Convert a ShapeMap to an sf Polygon map
#'
#' @param shapeMap A ShapeMap
#' @returns A new sf Polygon map
#' @importFrom sf st_sf st_sfc
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
