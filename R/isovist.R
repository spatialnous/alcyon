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
#' @return A ShapeMap with the isovist polygons
#' @export
isovist <- function(boundaryMap,
                    x,
                    y,
                    angle = NA,
                    viewAngle = NA,
                    verbose = FALSE) {
  isovistMap <- new("ShapeMap")
  isovistMap@ptr <- Rcpp_makeIsovists(
    boundaryMap@ptr,
    cbind(x, y),
    angle,
    viewAngle,
    verbose
  )

  return(isovistMap)
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
#' @param toY Y coordinate of the taget points
#' @param viewAngle The angle signifying the isovist's field of view
#' @param verbose Optional. Show more information of the process.
#' @return A ShapeMap with the isovist polygons
#' @export
isovist2pts <- function(boundaryMap,
                        x,
                        y,
                        toX,
                        toY,
                        viewAngle,
                        verbose = FALSE) {
  angles <- 180.0 * atan2(toY - y, toX - x) / pi
  angles <- ifelse(angles < 0.0, 360.0 + angles, angles)
  isovist(boundaryMap, x, y, angles, viewAngle, verbose)
}
