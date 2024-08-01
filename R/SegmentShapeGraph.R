# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

#' Segment ShapeGraph
#'
#' A representation of sala's Segment ShapeGraph in R. Holds onto a sala Segment
#' ShapeGraph pointer and operates on that
#' @name SegmentShapeGraph-class
#' @aliases SegmentShapeGraph
#' @family SegmentShapeGraph
#' @importFrom methods setOldClass
#' @exportClass SegmentShapeGraph
setOldClass(c("SegmentShapeGraph", "ShapeMap", "sf"))

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
        Rcpp_ShapeGraph_getSegmentConnections(attr(map, "sala_map"))
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
    newSegmentMapPtr <- Rcpp_axialToSegment(
        attr(axialShapeGraph, "sala_map"),
        "Segment Map",
        TRUE,
        stubRemoval
    )

    return(processPtrAsNewLineMap(
        newSegmentMapPtr,
        c("SegmentShapeGraph", "ShapeMap")
    ))
}

#' as("ShapeMap", "SegmentShapeGraph")
#'
#' This is a direct conversion, for ShapeMap -> Axial -> Segment see
#' \link{axialToSegmentShapeGraph}
#'
#' @name as
#' @family SegmentShapeGraph
#'
#' @importFrom methods as
setAs("ShapeMap", "SegmentShapeGraph", function(from) {
    class(from) <- c("SegmentShapeGraph", class(from))
    result <- Rcpp_shapeMapToSegment(attr(from, "sala_map"))
    return(processShapeMapResult(from, result))
})

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
    return(as(as(from, "ShapeMap"), "SegmentShapeGraph"))
})

#' Subset SegmentShapeGraph objects
#'
#' Subsetting SegmentShapeGraph objects essentially passes the data to sf.
#' See \link[sf]{sf}
#'
#' @name SegmentShapeGraph_subset
#' @param x object of class \code{SegmentShapeGraph} passed to \code{stars[]}
#' @param ... other parameters passed to \code{stars[]}
#' @export
`[.SegmentShapeGraph` <- function(x, ...) {
    class(x) <- setdiff(class(x), "SegmentShapeGraph")
    x <- NextMethod()
    class(x) <- c("SegmentShapeGraph", class(x))
    return(x)
}

#' @name SegmentShapeGraph_subset
#' @param x object of class \code{SegmentShapeGraph} passed to \code{stars[]}
#' @param ... other parameters passed to \code{stars[] <- }
#' @param value value to be passed to \code{sf[] <- }
#' @export
`[<-.SegmentShapeGraph` <- function(x, ..., value) {
    class(x) <- setdiff(class(x), "SegmentShapeGraph")
    x <- NextMethod()
    class(x) <- c("SegmentShapeGraph", class(x))
    return(x)
}
