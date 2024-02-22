# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

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

isovist2pts <- function(boundaryMap,
                        x,
                        y,
                        toX,
                        toY,
                        viewangle,
                        verbose = FALSE) {
  angles <- 180.0 * atan2(toY - y, toX - x) / pi
  angles <- ifelse(angles < 0.0, 360.0 + angles, angles)
  isovist(boundaryMap, x, y, angles, viewangle, verbose)
}
