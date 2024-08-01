# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

#' Axial ShapeGraph
#'
#' A representation of sala's Axial ShapeGraph in R. Holds onto a sala Axial
#' ShapeGraph pointer and operates on that
#' @name AxialShapeGraph-class
#' @aliases AxialShapeGraph
#' @family AxialShapeGraph
#' @importFrom methods setOldClass
#' @exportClass AxialShapeGraph
setOldClass(c("AxialShapeGraph", "ShapeMap", "sf"))

#' Get the Axial ShapeGraph connections
#'
#' @param map An Axial ShapeGraph
#' @returns A matrix with the connected refs
#' @docType methods
#' @importFrom methods setMethod
#' @eval c("@examples",
#' rxLoadSmallAxialLines(),
#' "connections(shapeGraph)")
#' @export
setMethod(
    "connections",
    signature = c(map = "AxialShapeGraph"),
    function(map) {
        Rcpp_ShapeGraph_getAxialConnections(attr(map, "sala_map"))
    }
)

#' Get the Axial ShapeGraph links
#'
#' @param map An Axial ShapeGraph
#' @returns A matrix with the linked refs
#' @docType methods
#' @importFrom methods setMethod
#' @eval c("@examples",
#' "# links of an axial map",
#' rxLoadSmallAxialLines(),
#' "linkRefs(shapeGraph, 0L, 9L)",
#' "unlinkCoords(shapeGraph, 530923.0, 184041.0, 530956.0, 183887.0)",
#' "links(shapeGraph)")
#' @export
setMethod(
    "links",
    signature = c(map = "AxialShapeGraph"),
    function(map) {
        Rcpp_ShapeGraph_getLinksUnlinks(attr(map, "sala_map"))
    }
)

#' Link two Axial Lines (coordinates)
#'
#' Link two locations on an Axial ShapeGraph using the point coordinates
#'
#' @param map An Axial ShapeGraph
#' @param fromX X coordinate of the first link point
#' @param fromY Y coordinate of the first link point
#' @param toX X coordinate of the second link point
#' @param toY Y coordinate of the second link point
#' @param copyMap Optional. Copy the internal sala map
#' @returns A new Axial ShapeGraph with linked lines
#' @docType methods
#' @importFrom methods setMethod
#' @eval c("@examples",
#' rxLoadSmallAxialLines(),
#' "linkCoords(shapeGraph, 982.8, -1620.3, 1217.1, -1977.3)")
#' @export
setMethod(
    "linkCoords",
    signature = c(map = "AxialShapeGraph"),
    function(map, fromX, fromY, toX, toY, copyMap = TRUE) {
        result <- Rcpp_ShapeGraph_linkCoords(
            attr(map, "sala_map"),
            cbind(fromX, fromY, toX, toY),
            copyMapNV = copyMap
        )
        return(processShapeMapResult(map, result))
    }
)

#' Unlink two Axial Lines (coordinates)
#'
#' Unlink two locations on an Axial ShapeGraph using the point coordinates
#'
#' @param map An Axial ShapeGraph
#' @param fromX X coordinate of the first unlink point
#' @param fromY Y coordinate of the first unlink point
#' @param toX X coordinate of the second unlink point
#' @param toY Y coordinate of the second unlink point
#' @param copyMap Optional. Copy the internal sala map
#' @returns A new Axial ShapeGraph with unlinked lines
#' @docType methods
#' @importFrom methods setMethod
#' @eval c("@examples",
#' rxLoadSmallAxialLines(),
#' "unlinkCoords(shapeGraph, 982.8, -1620.3, 1080.4, -1873.5)")
#' @export
setMethod(
    "unlinkCoords",
    signature = c(map = "AxialShapeGraph"),
    function(map, fromX, fromY, toX, toY, copyMap = TRUE) {
        result <- Rcpp_ShapeGraph_unlinkCoords(
            attr(map, "sala_map"),
            cbind(fromX, fromY, toX, toY),
            copyMapNV = copyMap
        )
        return(processShapeMapResult(map, result))
    }
)

#' Link two Axial Lines (refs)
#'
#' Link two lines on an Axial ShapeGraph using their refs
#'
#' @param map An Axial ShapeGraph
#' @param fromRef Ref of the first link line
#' @param toRef Ref of the second link line
#' @param copyMap Optional. Copy the internal sala map
#' @returns A new Axial ShapeGraph with linked lines
#' @docType methods
#' @importFrom methods setMethod
#' @eval c("@examples",
#' rxLoadSmallAxialLines(),
#' "linkRefs(shapeGraph, 0L, 9L)")
#' @export
setMethod(
    "linkRefs",
    signature = c(map = "AxialShapeGraph"),
    function(map, fromRef, toRef, copyMap = TRUE) {
        result <- Rcpp_ShapeGraph_linkRefs(
            attr(map, "sala_map"),
            cbind(fromRef, toRef),
            copyMapNV = copyMap
        )
        return(processShapeMapResult(map, result))
    }
)

#' Unlink two Axial Lines (refs)
#'
#' Unlink two lines on an Axial ShapeGraph using their refs
#'
#' @param map An Axial ShapeGraph
#' @param fromRef Ref of the first unlink line
#' @param toRef Ref of the second unlink line
#' @param copyMap Optional. Copy the internal sala map
#' @returns A new Axial ShapeGraph with unlinked lines
#' @docType methods
#' @importFrom methods setMethod
#' @eval c("@examples",
#' rxLoadSmallAxialLines(),
#' "unlinkRefs(shapeGraph, 12L, 34L)")
#' @export
setMethod(
    "unlinkRefs",
    signature = c(map = "AxialShapeGraph"),
    function(map, fromRef, toRef, copyMap = TRUE) {
        result <- Rcpp_ShapeGraph_unlinkRefs(
            attr(map, "sala_map"),
            cbind(fromRef, toRef),
            copyMapNV = copyMap
        )
        return(processShapeMapResult(map, result))
    }
)


#' Unlink two Axial Lines (crosspoint)
#'
#' Unlink two crossing lines on an Axial ShapeGraph at the crossing point
#'
#' @param map An Axial ShapeGraph
#' @param x X coordinate of the unlink crossing point
#' @param y Y coordinate of the unlink crossing point
#' @param copyMap Optional. Copy the internal sala map
#' @returns A new Axial ShapeGraph with unlinked lines
#' @docType methods
#' @importFrom methods setMethod
#' @eval c("@examples",
#' rxLoadSmallAxialLines(),
#' "unlinkAtCrossPoint(shapeGraph, 530925.0, 184119.0)")
#' @export
setMethod(
    "unlinkAtCrossPoint",
    signature = c(map = "AxialShapeGraph"),
    function(map, x, y, copyMap = TRUE) {
        result <- Rcpp_ShapeGraph_unlinkAtCrossPoint(
            attr(map, "sala_map"),
            cbind(x, y),
            copyMapNV = copyMap
        )
        return(processShapeMapResult(map, result))
    }
)

#' as("ShapeMap", "AxialShapeGraph")
#'
#' @name as
#' @family AxialShapeGraph
#'
#' @importFrom methods as
setAs("ShapeMap", "AxialShapeGraph", function(from) {
    class(from) <- c("AxialShapeGraph", class(from))
    result <- Rcpp_toAxialShapeGraph(attr(from, "sala_map"))
    return(processShapeMapResult(from, result))
})

#' as("sf", "AxialShapeGraph")
#'
#' @name as
#' @family AxialShapeGraph
#'
#' @importFrom methods as
setAs("sf", "AxialShapeGraph", function(from) {
    return(as(as(from, "ShapeMap"), "AxialShapeGraph"))
})

#' Subset AxialShapeGraph objects
#'
#' Subsetting AxialShapeGraph objects essentially passes the data to sf.
#' See \link[sf]{sf}
#'
#' @name AxialShapeGraph_subset
#' @param x object of class \code{AxialShapeGraph} passed to \code{stars[]}
#' @param ... other parameters passed to \code{stars[]}
#' @export
`[.AxialShapeGraph` <- function(x, ...) {
    class(x) <- setdiff(class(x), "AxialShapeGraph")
    x <- NextMethod()
    class(x) <- c("AxialShapeGraph", class(x))
    return(x)
}

#' @name AxialShapeGraph_subset
#' @param x object of class \code{AxialShapeGraph} passed to \code{stars[]}
#' @param ... other parameters passed to \code{stars[] <- }
#' @param value value to be passed to \code{sf[] <- }
#' @export
`[<-.AxialShapeGraph` <- function(x, ..., value) {
    class(x) <- setdiff(class(x), "AxialShapeGraph")
    x <- NextMethod()
    class(x) <- c("AxialShapeGraph", class(x))
    return(x)
}
