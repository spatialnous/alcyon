# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only


#' PointMap
#'
#' A representation of sala's PointMap in R. Holds onto a sala PointMap pointer
#' and operates on that
#' @importFrom methods setClass setMethod
setClass("PointMap", slots = c(
  ptr = "externalptr"
))

# lintr seems unable to understand this as a constructor
# thus we have to exclude it from the particular linter
#' @importFrom methods new
PointMap <- function(name, gridSize) { # nolint: object_name_linter
  new("PointMap", ptr = Rcpp_PointMap_createFromGrid(name, gridSize))
}

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
    Rcpp_PointMap_getName(map@ptr)
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
#' "connections(pointMap)")
#' @export
setMethod(
  "connections",
  signature = c(map = "PointMap"),
  function(map) {
    return(Rcpp_PointMap_getConnections(map@ptr))
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
    return(Rcpp_PointMap_getLinks(map@ptr))
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
#' @returns None
#' @docType methods
#' @importFrom methods setMethod
#' @eval c("@examples",
#' rxLoadInteriorLinesAsPointMap(),
#' "linkCoords(pointMap, 1.74, 6.7, 5.05, 5.24)")
#' @export
setMethod(
  "linkCoords",
  signature = c(map = "PointMap"),
  function(map, fromX, fromY, toX, toY) {
    Rcpp_PointMap_linkCoords(
      map@ptr,
      cbind(fromX, fromY, toX, toY)
    )
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
#' @returns None
#' @docType methods
#' @importFrom methods setMethod
#' @eval c("@examples",
#' rxLoadInteriorLinesAsPointMap(),
#' "linkCoords(pointMap, 1.74, 6.7, 5.05, 5.24)",
#' "unlinkCoords(pointMap, 1.74, 6.7, 5.05, 5.24)")
#' @export
setMethod(
  "unlinkCoords",
  signature = c(map = "PointMap"),
  function(map, fromX, fromY, toX, toY) {
    Rcpp_PointMap_unlinkCoords(
      map@ptr,
      cbind(fromX, fromY, toX, toY)
    )
  }
)

#' Link two PointMap Cells (refs)
#'
#' Link two cells on an PointMap using their refs
#'
#' @param map A PointMap
#' @param fromRef Ref of the first link line
#' @param toRef Ref of the second link line
#' @returns None
#' @docType methods
#' @importFrom methods setMethod
#' @eval c("@examples",
#' rxLoadInteriorLinesAsPointMap(),
#' "linkRefs(pointMap, 1835056L, 7208971L)")
#' @export
setMethod(
  "linkRefs",
  signature = c(map = "PointMap"),
  function(map, fromRef, toRef) {
    Rcpp_PointMap_linkRefs(
      map@ptr,
      cbind(fromRef, toRef)
    )
  }
)

#' Unlink two PointMap Cells (refs)
#'
#' Unlink two cells on an PointMap using their refs
#'
#' @param map A PointMap
#' @param fromRef Ref of the first unlink line
#' @param toRef Ref of the second unlink line
#' @returns None
#' @docType methods
#' @importFrom methods setMethod
#' @eval c("@examples",
#' rxLoadInteriorLinesAsPointMap(),
#' "linkRefs(pointMap, 1835056L, 7208971L)",
#' "unlinkRefs(pointMap, 1835056L, 7208971L)")
#' @export
setMethod(
  "unlinkRefs",
  signature = c(map = "PointMap"),
  function(map, fromRef, toRef) {
    Rcpp_PointMap_unlinkRefs(
      map@ptr,
      cbind(fromRef, toRef)
    )
  }
)

#' as("PointMap", "SpatialPointsDataFrame")
#'
#' @name as
#' @family PointMap
#'
#' @importFrom methods as
#' @importClassesFrom sp SpatialPixelsDataFrame
#' @importFrom sp SpatialPointsDataFrame
#' @importMethodsFrom sp gridded<-
setAs("PointMap", "SpatialPixelsDataFrame", function(from) {
  coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = from@ptr)
  map <- SpatialPointsDataFrame(coords[, c(1L, 2L)],
    data = data.frame(coords,
      check.names = FALSE
    )
  )
  gridded(map) <- TRUE
  return(map)
})
