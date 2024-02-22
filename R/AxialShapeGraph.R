# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

# A representation of sala's Axial ShapeGraph in R. Holds onto a sala Axial
# ShapeGraph pointer and operates on that

setClass("AxialShapeGraph", slots = c(
  ptr = "externalptr"
))

setGeneric("name", function(x) standardGeneric("name"))

setMethod("name", "AxialShapeGraph", function(x) {
  Rcpp_ShapeMap_getName(x@ptr)
})
