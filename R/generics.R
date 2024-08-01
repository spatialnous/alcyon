# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

#' Get map name
#' @param map A sala map
#' @returns The name of the object as a string
#' @importFrom methods setGeneric
setGeneric(
    "name",
    function(map) standardGeneric("name")
)

#' Get map connections
#' @param map A sala map
#' @returns A matrix with the connected refs
#' @importFrom methods setGeneric
setGeneric(
    "connections",
    function(map) standardGeneric("connections")
)

#' Get map links
#' @param map A sala map
#' @returns A matrix with the linked refs
#' @importFrom methods setGeneric
#' @importFrom methods .valueClassTest
setGeneric(
    "links",
    function(map) standardGeneric("links"),
    valueClass = "matrix"
)

#' Link map points/lines as if selecting them using points
#' @param map A sala map
#' @param fromX X coordinate of the origin point
#' @param fromY Y coordinate of the origin point
#' @param toX X coordinate of the target point
#' @param toY Y coordinate of the target point
#' @param copyMap Optional. Copy the internal sala map
#' @returns A new map with linked points/lines
#' @importFrom methods setGeneric
setGeneric(
    "linkCoords",
    function(map, fromX, fromY, toX, toY, copyMap = TRUE) {
        standardGeneric("linkCoords")
    }
)

#' Unlink map points/lines as if selecting them using points
#' @param map A sala map
#' @param fromX X coordinate of the origin point
#' @param fromY Y coordinate of the origin point
#' @param toX X coordinate of the target point
#' @param toY Y coordinate of the target point
#' @param copyMap Optional. Copy the internal sala map
#' @returns A new map with unlinked points/lines
#' @importFrom methods setGeneric
setGeneric(
    "unlinkCoords",
    function(map, fromX, fromY, toX, toY, copyMap = TRUE) {
        standardGeneric("unlinkCoords")
    }
)

#' Link map points/lines using their refs
#' @param map A sala map
#' @param fromRef The ref of the origin element
#' @param toRef The ref of the target element
#' @param copyMap Optional. Copy the internal sala map
#' @returns A new map with linked points/lines
#' @importFrom methods setGeneric
setGeneric(
    "linkRefs",
    function(map, fromRef, toRef, copyMap = TRUE) {
        standardGeneric("linkRefs")
    }
)

#' Unlink map points/lines using their refs
#' @param map A sala map
#' @param fromRef The ref of the origin element
#' @param toRef The ref of the target element
#' @param copyMap Optional. Copy the internal sala map
#' @returns A new map with unlinked points/lines
#' @importFrom methods setGeneric
setGeneric(
    "unlinkRefs",
    function(map, fromRef, toRef, copyMap = TRUE) {
        standardGeneric("unlinkRefs")
    }
)

#' Unlink map lines at their crossing point
#' @param map A sala map
#' @param x X coordinate of the crossing point
#' @param y Y coordinate of the crossing point
#' @param copyMap Optional. Copy the internal sala map
#' @returns A new map with linked lines
#' @importFrom methods setGeneric
setGeneric(
    "unlinkAtCrossPoint",
    function(map, x, y, copyMap = TRUE) {
        standardGeneric("unlinkAtCrossPoint")
    }
)
