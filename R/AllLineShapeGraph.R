# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

# A representation of sala's All-line ShapeGraph in R. Holds onto a sala
# All-line ShapeGraph pointer and operates on that

setClass("AllLineShapeGraph", slots = c(
  ptr = "externalptr"
))

setGeneric("name", function(x) standardGeneric("name"))

setMethod("name", "AllLineShapeGraph", function(x) {
  Rcpp_ShapeMap_getName(x@ptr)
})
