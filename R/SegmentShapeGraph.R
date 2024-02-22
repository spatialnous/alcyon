# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

# A representation of sala's Segment ShapeGraph in R. Holds onto a sala Segment
# ShapeGraph pointer and operates on that

setClass("SegmentShapeGraph", slots = c(
  ptr = "externalptr"
))

setGeneric("name", function(x) standardGeneric("name"))

setMethod("name", "SegmentShapeGraph", function(x) {
  Rcpp_ShapeMap_getName(x@ptr)
})
