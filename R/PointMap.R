# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only


#' PointMap
#'
#' A representation of sala's PointMap in R. Holds onto a sala PointMap pointer
#' and operates on that
#' @name PointMap-class
#' @aliases PointMap
#' @family PointMap
#' @importFrom methods setOldClass
#' @exportClass PointMap
setOldClass(c("PointMap", "stars"))

#' Get the PointMap name
#'
#' @param map A PointMap
#' @returns The name of the PointMap as a string
#' @docType methods
#' @importFrom methods setMethod
#' @eval c("@examples",
#' rxLoadInteriorLinesAsPointMap(),
#' "name(pointMap)")
#' @export
setMethod(
    "name",
    signature = c(map = "PointMap"),
    function(map) {
        Rcpp_PointMap_getName(attr(map, "sala_map"))
    }
)

#' Get the PointMap connections
#'
#' @param map A PointMap
#' @returns A matrix with the connected refs
#' @docType methods
#' @importFrom methods setMethod
#' @eval c("@examples",
#' rxLoadInteriorLinesAsPointMap(),
#' "# plot the first 100 connections only",
#' "head(connections(pointMap), 100)")
#' @export
setMethod(
    "connections",
    signature = c(map = "PointMap"),
    function(map) {
        return(Rcpp_PointMap_getConnections(attr(map, "sala_map")))
    }
)

#' Get the PointMap links
#'
#' @param map A PointMap
#' @returns A matrix with the linked refs
#' @docType methods
#' @importFrom methods setMethod
#' @eval c("@examples",
#' rxLoadInteriorLinesAsPointMap(),
#' "linkRefs(pointMap, 1835056L, 7208971L)",
#' "links(pointMap)")
#' @export
setMethod(
    "links",
    signature = c(map = "PointMap"),
    function(map) {
        return(Rcpp_PointMap_getLinks(attr(map, "sala_map")))
    }
)

#' Link two PointMap Cells (coordinates)
#'
#' Link two cells on a PointMap using the point coordinates
#'
#' @param map A PointMap
#' @param fromX X coordinate of the first link point
#' @param fromY Y coordinate of the first link point
#' @param toX X coordinate of the second link point
#' @param toY Y coordinate of the second link point
#' @param copyMap Optional. Copy the internal sala map
#' @returns A new PointMap with linked points
#' @docType methods
#' @importFrom methods setMethod
#' @eval c("@examples",
#' rxLoadInteriorLinesAsPointMap(),
#' "linkCoords(pointMap, 1.74, 6.7, 5.05, 5.24)")
#' @export
setMethod(
    "linkCoords",
    signature = c(map = "PointMap"),
    function(map, fromX, fromY, toX, toY, copyMap = TRUE) {
        result <- Rcpp_PointMap_linkCoords(
            attr(map, "sala_map"),
            cbind(fromX, fromY, toX, toY),
            copyMapNV = copyMap
        )
        return(processPointMapResult(map, result))
    }
)

#' Unlink two PointMap Cells (coordinates)
#'
#' Unlink two cells on a PointMap using the point coordinates
#'
#' @param map A PointMap
#' @param fromX X coordinate of the first unlink point
#' @param fromY Y coordinate of the first unlink point
#' @param toX X coordinate of the second unlink point
#' @param toY Y coordinate of the second unlink point
#' @param copyMap Optional. Copy the internal sala map
#' @returns A new PointMap with unlinked points
#' @docType methods
#' @importFrom methods setMethod
#' @eval c("@examples",
#' rxLoadInteriorLinesAsPointMap(),
#' "pointMap <- linkCoords(pointMap, 1.74, 6.7, 5.05, 5.24)",
#' "pointMap <- unlinkCoords(pointMap, 1.74, 6.7, 5.05, 5.24)")
#' @export
setMethod(
    "unlinkCoords",
    signature = c(map = "PointMap"),
    function(map, fromX, fromY, toX, toY, copyMap = TRUE) {
        result <- Rcpp_PointMap_unlinkCoords(
            attr(map, "sala_map"),
            cbind(fromX, fromY, toX, toY),
            copyMapNV = copyMap
        )
        return(processPointMapResult(map, result))
    }
)

#' Link two PointMap Cells (refs)
#'
#' Link two cells on an PointMap using their refs
#'
#' @param map A PointMap
#' @param fromRef Ref of the first link line
#' @param toRef Ref of the second link line
#' @param copyMap Optional. Copy the internal sala map
#' @returns A new PointMap with linked points
#' @docType methods
#' @importFrom methods setMethod
#' @eval c("@examples",
#' rxLoadInteriorLinesAsPointMap(),
#' "pointMap <- linkRefs(pointMap, 1835056L, 7208971L)")
#' @export
setMethod(
    "linkRefs",
    signature = c(map = "PointMap"),
    function(map, fromRef, toRef, copyMap = TRUE) {
        result <- Rcpp_PointMap_linkRefs(
            attr(map, "sala_map"),
            cbind(fromRef, toRef),
            copyMapNV = copyMap
        )
        return(processPointMapResult(map, result))
    }
)

#' Unlink two PointMap Cells (refs)
#'
#' Unlink two cells on an PointMap using their refs
#'
#' @param map A PointMap
#' @param fromRef Ref of the first unlink line
#' @param toRef Ref of the second unlink line
#' @param copyMap Optional. Copy the internal sala map
#' @returns A new PointMap with unlinked points
#' @docType methods
#' @importFrom methods setMethod
#' @eval c("@examples",
#' rxLoadInteriorLinesAsPointMap(),
#' "pointMap <- linkRefs(pointMap, 1835056L, 7208971L)",
#' "pointMap <- unlinkRefs(pointMap, 1835056L, 7208971L)")
#' @export
setMethod(
    "unlinkRefs",
    signature = c(map = "PointMap"),
    function(map, fromRef, toRef, copyMap = TRUE) {
        result <- Rcpp_PointMap_unlinkRefs(
            attr(map, "sala_map"),
            cbind(fromRef, toRef),
            copyMapNV = copyMap
        )
        return(processPointMapResult(map, result))
    }
)

#' Subset PointMap objects
#'
#' Subsetting PointMap objects essentially passes the data to stars
#' See \link[stars]{stars_subset}
#'
#' @name PointMap_subset
#' @param x object of class \code{PointMap} passed to \code{stars[]}
#' @param ... other parameters passed to \code{stars[]}
#' @export
`[.PointMap` <- function(x, ...) {
    class(x) <- setdiff(class(x), "PointMap")
    x <- NextMethod()
    class(x) <- c("PointMap", class(x))
    return(x)
}

#' @name PointMap_subset
#' @param x object of class \code{PointMap} passed to \code{stars[]}
#' @param ... other parameters passed to \code{stars[] <- }
#' @param value value to be passed to \code{stars[] <- }
#' @export
`[<-.PointMap` <- function(x, ..., value) {
    class(x) <- setdiff(class(x), "PointMap")
    x <- NextMethod()
    class(x) <- c("PointMap", class(x))
    return(x)
}

#' plot a PointMap
#'
#' Calls a standard plot.stars, but flips the first argument around the x axis
#' @param x object of class \code{PointMap}
#' @param ... other parameters passed to \code{stars[]}
#' @importFrom stars st_flip
#' @export
plot.PointMap <- function(x, ...) {
    x <- st_flip(x, which = 2L)
    NextMethod(...)
}
