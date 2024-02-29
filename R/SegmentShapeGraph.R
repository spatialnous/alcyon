# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

#' Segment ShapeGraph
#'
#' A representation of sala's Segment ShapeGraph in R. Holds onto a sala Segment
#' ShapeGraph pointer and operates on that
#' @importFrom methods setClass
setClass("SegmentShapeGraph", contains = "ShapeGraph")

#' Get the Segment ShapeGraph connections
#'
#' @param map An Segment ShapeGraph
#' @returns A matrix with the connected refs
#' @docType methods
#' @importFrom methods setMethod
#' @export
setMethod(
  "connections",
  signature(map = "SegmentShapeGraph"),
  function(map) {
    Rcpp_ShapeGraph_getSegmentConnections(map@ptr)
  }
)

#' as("sf", "SegmentShapeGraph")
#'
#' This is a direct conversion, for ShapeMap -> Axial -> Segment see
#' \link{axialToSegmentShapeGraph}
#'
#' @name as
#' @family SegmentShapeGraph
#'
#' @importFrom methods as
setAs("sf", "SegmentShapeGraph", function(from) {
  shapeGraph <- new("SegmentShapeGraph")
  shapeMap <- as(from, "ShapeMap")
  shapeGraph@ptr <- Rcpp_shapeMapToSegment(shapeMap@ptr)
  return(shapeGraph)
})
