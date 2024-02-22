# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

vgaThroughVision <- function(pointMap) {
  Rcpp_VGA_throughVision(pointMap@ptr)
}

vgaAngular <- function(pointMap, radius, gatesOnly) {
  Rcpp_VGA_angular(pointMap@ptr, radius, gatesOnly)
}

vgaMetric <- function(pointMap, radius, gatesOnly) {
  Rcpp_VGA_metric(pointMap@ptr, radius, gatesOnly)
}

vgaVisualGlobal <- function(pointMap, radius, gatesOnly) {
  Rcpp_VGA_visualGlobal(pointMap@ptr, radius, gatesOnly)
}

vgaVisualLocal <- function(pointMap, gatesOnly) {
  Rcpp_VGA_visualLocal(pointMap@ptr, gatesOnly)
}

vgaIsovist <- function(pointMap, boundaryMap) {
  Rcpp_VGA_isovist(pointMap@ptr, boundaryMap@ptr)
}
