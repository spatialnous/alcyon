# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only


#' LatticeMap
#'
#' A representation of sala's LatticeMap in R. Holds onto a sala LatticeMap pointer
#' and operates on that
#' @name LatticeMap-class
#' @aliases LatticeMap
#' @family LatticeMap
#' @importFrom methods setOldClass
#' @exportClass LatticeMap
setOldClass(c("LatticeMap", "stars"))

#' Get the LatticeMap name
#'
#' @param map A LatticeMap
#' @returns The name of the LatticeMap as a string
#' @docType methods
#' @importFrom methods setMethod
#' @eval c("@examples",
#' rxLoadFloorLinesAsLatticeMap(),
#' "name(latticeMap)")
#' @export
setMethod(
    "name",
    signature = c(map = "LatticeMap"),
    function(map) {
        Rcpp_LatticeMap_getName(attr(map, "sala_map"))
    }
)

#' Get the LatticeMap connections
#'
#' @param map A LatticeMap
#' @returns A matrix with the connected refs
#' @docType methods
#' @importFrom methods setMethod
#' @eval c("@examples",
#' rxLoadFloorLinesAsLatticeMap(),
#' "# plot the first 100 connections only",
#' "head(connections(latticeMap), 100)")
#' @export
setMethod(
    "connections",
    signature = c(map = "LatticeMap"),
    function(map) {
        return(Rcpp_LatticeMap_getConnections(attr(map, "sala_map")))
    }
)

#' Get the LatticeMap links
#'
#' @param map A LatticeMap
#' @returns A matrix with the linked refs
#' @docType methods
#' @importFrom methods setMethod
#' @eval c("@examples",
#' rxLoadFloorLinesAsLatticeMap(),
#' "linkRefs(latticeMap, 1835056L, 7208971L)",
#' "links(latticeMap)")
#' @export
setMethod(
    "links",
    signature = c(map = "LatticeMap"),
    function(map) {
        return(Rcpp_LatticeMap_getLinks(attr(map, "sala_map")))
    }
)

#' Link two LatticeMap Cells (coordinates)
#'
#' Link two cells on a LatticeMap using the point coordinates
#'
#' @param map A LatticeMap
#' @param fromX X coordinate of the first link point
#' @param fromY Y coordinate of the first link point
#' @param toX X coordinate of the second link point
#' @param toY Y coordinate of the second link point
#' @param copyMap Optional. Copy the internal sala map
#' @returns A new LatticeMap with linked points
#' @docType methods
#' @importFrom methods setMethod
#' @eval c("@examples",
#' rxLoadFloorLinesAsLatticeMap(),
#' "linkCoords(latticeMap, 1.74, 6.7, 5.05, 5.24)")
#' @export
setMethod(
    "linkCoords",
    signature = c(map = "LatticeMap"),
    function(map, fromX, fromY, toX, toY, copyMap = TRUE) {
        result <- Rcpp_LatticeMap_linkCoords(
            attr(map, "sala_map"),
            cbind(fromX, fromY, toX, toY),
            copyMapNV = copyMap
        )
        return(processLatticeMapResult(map, result))
    }
)

#' Unlink two LatticeMap Cells (coordinates)
#'
#' Unlink two cells on a LatticeMap using the point coordinates
#'
#' @param map A LatticeMap
#' @param fromX X coordinate of the first unlink point
#' @param fromY Y coordinate of the first unlink point
#' @param toX X coordinate of the second unlink point
#' @param toY Y coordinate of the second unlink point
#' @param copyMap Optional. Copy the internal sala map
#' @returns A new LatticeMap with unlinked points
#' @docType methods
#' @importFrom methods setMethod
#' @eval c("@examples",
#' rxLoadFloorLinesAsLatticeMap(),
#' "latticeMap <- linkCoords(latticeMap, 1.74, 6.7, 5.05, 5.24)",
#' "latticeMap <- unlinkCoords(latticeMap, 1.74, 6.7, 5.05, 5.24)")
#' @export
setMethod(
    "unlinkCoords",
    signature = c(map = "LatticeMap"),
    function(map, fromX, fromY, toX, toY, copyMap = TRUE) {
        result <- Rcpp_LatticeMap_unlinkCoords(
            attr(map, "sala_map"),
            cbind(fromX, fromY, toX, toY),
            copyMapNV = copyMap
        )
        return(processLatticeMapResult(map, result))
    }
)

#' Link two LatticeMap Cells (refs)
#'
#' Link two cells on an LatticeMap using their refs
#'
#' @param map A LatticeMap
#' @param fromRef Ref of the first link line
#' @param toRef Ref of the second link line
#' @param copyMap Optional. Copy the internal sala map
#' @returns A new LatticeMap with linked points
#' @docType methods
#' @importFrom methods setMethod
#' @eval c("@examples",
#' rxLoadFloorLinesAsLatticeMap(),
#' "latticeMap <- linkRefs(latticeMap, 1835056L, 7208971L)")
#' @export
setMethod(
    "linkRefs",
    signature = c(map = "LatticeMap"),
    function(map, fromRef, toRef, copyMap = TRUE) {
        result <- Rcpp_LatticeMap_linkRefs(
            attr(map, "sala_map"),
            cbind(fromRef, toRef),
            copyMapNV = copyMap
        )
        return(processLatticeMapResult(map, result))
    }
)

#' Unlink two LatticeMap Cells (refs)
#'
#' Unlink two cells on an LatticeMap using their refs
#'
#' @param map A LatticeMap
#' @param fromRef Ref of the first unlink line
#' @param toRef Ref of the second unlink line
#' @param copyMap Optional. Copy the internal sala map
#' @returns A new LatticeMap with unlinked points
#' @docType methods
#' @importFrom methods setMethod
#' @eval c("@examples",
#' rxLoadFloorLinesAsLatticeMap(),
#' "latticeMap <- linkRefs(latticeMap, 1835056L, 7208971L)",
#' "latticeMap <- unlinkRefs(latticeMap, 1835056L, 7208971L)")
#' @export
setMethod(
    "unlinkRefs",
    signature = c(map = "LatticeMap"),
    function(map, fromRef, toRef, copyMap = TRUE) {
        result <- Rcpp_LatticeMap_unlinkRefs(
            attr(map, "sala_map"),
            cbind(fromRef, toRef),
            copyMapNV = copyMap
        )
        return(processLatticeMapResult(map, result))
    }
)

#' Subset LatticeMap objects
#'
#' Subsetting LatticeMap objects essentially passes the data to stars
#' See \link[stars]{stars_subset}
#'
#' @name LatticeMap_subset
#' @param x object of class \code{LatticeMap} passed to \code{stars[]}
#' @param ... other parameters passed to \code{stars[]}
#' @export
`[.LatticeMap` <- function(x, ...) {
    class(x) <- setdiff(class(x), "LatticeMap")
    x <- NextMethod()
    class(x) <- c("LatticeMap", class(x))
    return(x)
}

#' @name LatticeMap_subset
#' @param x object of class \code{LatticeMap} passed to \code{stars[]}
#' @param ... other parameters passed to \code{stars[] <- }
#' @param value value to be passed to \code{stars[] <- }
#' @export
`[<-.LatticeMap` <- function(x, ..., value) {
    class(x) <- setdiff(class(x), "LatticeMap")
    x <- NextMethod()
    class(x) <- c("LatticeMap", class(x))
    return(x)
}

#' plot a LatticeMap
#'
#' Calls a standard plot.stars, but flips the first argument around the x axis
#' @param x object of class \code{LatticeMap}
#' @param ... other parameters passed to \code{stars[]}
#' @importFrom stars st_flip
#' @export
plot.LatticeMap <- function(x, ...) {
    x <- st_flip(x, which = 2L)
    NextMethod(...)
}
