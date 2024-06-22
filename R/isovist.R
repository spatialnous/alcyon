# SPDX-FileCopyrightText: 2019-2023 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

#' Create isovists at point and direction angle
#'
#' Create one or more isovists at particular points, given angle and field of
#' view
#'
#' @param boundaryMap A ShapeMap with lines designating the isovist boundaries
#' @param x X coordinate of the origin points
#' @param y Y coordinate of the origin points
#' @param angle The angle (from the X axis) of the isovist look direction
#' @param viewAngle The angle signifying the isovist's field of view
#' @param verbose Optional. Show more information of the process.
#' @returns A ShapeMap with the isovist polygons
#' @eval c("@examples",
#' rxLoadSimpleLinesAsShapeMap(),
#' "isovist(",
#' "  shapeMap,",
#' "  x = c(3.01, 1.3),",
#' "  y = c(6.70, 5.2),",
#' "  angle = 0.01,",
#' "  viewAngle = 3.14,",
#' "  FALSE",
#' ")")
#' @export
isovist <- function(boundaryMap,
                    x,
                    y,
                    angle = NA,
                    viewAngle = NA,
                    verbose = FALSE) {
  isovistMapPtr <- Rcpp_makeIsovists(
    attr(boundaryMap, "sala_map"),
    cbind(x, y),
    angle,
    viewAngle,
    verbose
  )

  return(processPtrAsNewPolyMap(isovistMapPtr, "ShapeMap"))
}

#' Create isovists using two points
#'
#' Create one or more isovists at particular points, given another point for
#' direction and an angle for field of view
#'
#' @param boundaryMap A ShapeMap with lines designating the isovist boundaries
#' @param x X coordinate of the origin points
#' @param y Y coordinate of the origin points
#' @param toX X coordinate of the target points
#' @param toY Y coordinate of the target points
#' @param viewAngle The angle signifying the isovist's field of view
#' @param verbose Optional. Show more information of the process.
#' @returns A ShapeMap with the isovist polygons
#' @eval c("@examples",
#' rxLoadSimpleLinesAsShapeMap(),
#' "isovist2pts(",
#' "  shapeMap,",
#' "  x = c(3.01, 1.3),",
#' "  y = c(6.70, 5.2),",
#' "  toX = c(3.40, 1.1),",
#' "  toY = c(6.50, 5.6),",
#' "  viewAngle = 3.14,",
#' "  FALSE",
#' ")")
#' @export
isovist2pts <- function(boundaryMap,
                        x,
                        y,
                        toX,
                        toY,
                        viewAngle,
                        verbose = FALSE) {
  angles <- atan2(toY - y, toX - x)
  angles <- ifelse(angles < 0.0, (2.0 * pi) + angles, angles)
  isovist(boundaryMap, x, y, angles, viewAngle, verbose)
}
