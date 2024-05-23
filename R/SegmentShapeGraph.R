# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

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
#' @eval c("@examples",
#' rxLoadSmallSegmentLines(),
#' "connections(shapeGraph)")
#' @export
setMethod(
  "connections",
  signature(map = "SegmentShapeGraph"),
  function(map) {
    Rcpp_ShapeGraph_getSegmentConnections(map@ptr)
  }
)

#' Axial to Segment ShapeGraph
#'
#' Convert an Axial ShapeGraph to a Segment ShapeGraph
#'
#' @param axialShapeGraph An Axial ShapeGraph
#' @param stubRemoval Remove stubs of axial lines shorter than this
#' percentage (for example provide 0.4 for 40\%)
#' @returns A new Segment ShapeGraph
#' @importFrom methods new
#' @eval c("@examples",
#' rxLoadSmallAxialLines(),
#' "axialToSegmentShapeGraph(shapeGraph, stubRemoval = 0.4)")
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
