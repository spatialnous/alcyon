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
    return(processPtrAsNewPolyMap(attr(shapeMap, "sala_map"), "ShapeMap"))
}
