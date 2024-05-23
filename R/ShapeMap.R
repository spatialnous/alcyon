# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

# A representation of sala's ShapeMap in R. Holds onto a sala Shapemap
# pointer and operates on that
#' @importFrom methods setClass
setClass("ShapeMap", slots = c(
  ptr = "externalptr"
))

# lintr seems unable to understand this as a contstructor
# thus we have to exclude it from the particular linter
#' @importFrom methods new
ShapeMap <- function(name) { # nolint: object_name_linter
  new("ShapeMap", ptr = Rcpp_ShapeMap_make(name))
}

#' Get the ShapeMap name
#'
#' @param map A ShapeMap
#' @returns The name of the ShapeMap as a string
#' @importFrom methods setMethod
#' @eval c("@examples",
#' rxLoadSimpleLinesAsShapeMap(),
#' "name(shapeMap)")
#' @export
setMethod(
  "name",
  signature(map = "ShapeMap"),
  function(map) {
    Rcpp_ShapeMap_getName(map@ptr)
  }
)

#' as("sf", "ShapeMap")
#'
#' @name as
#' @family ShapeMap
#'
#' @importFrom methods as
setAs("sf", "ShapeMap", function(from) {
  shapeMap <- new("ShapeMap")
  cols <- names(from)[names(from) != "geometry"]
  if (length(cols) != 0L) {
    numericCols <- unlist(lapply(cols, function(col) {
      is.numeric(from[[col]])
    }))
    if (!all(numericCols)) {
      warning(
        "Non-numeric columns will not be transferred to ",
        "the ShapeMap: ",
        do.call(paste, as.list(cols[!numericCols]))
      )
    }
    shapeMap@ptr <- Rcpp_toShapeMap(from, which(numericCols))
  } else {
    shapeMap@ptr <- Rcpp_toShapeMap(from)
  }
  return(shapeMap)
})

#' as("ShapeMap", "sf")
#'
#' @name as
#' @family ShapeMap
#' @importFrom sf st_sf st_sfc
#' @importFrom methods as
setAs("ShapeMap", "sf", function(from) {
  coords <- Rcpp_ShapeMap_getShapesAsLineCoords(from@ptr)
  sfGeom <- st_sfc(lapply(seq_len(nrow(coords)), function(rowIdx) {
    sf::st_linestring(
      matrix(coords[rowIdx, ], ncol = 2L, byrow = TRUE),
      dim = "XY"
    )
  }))
  attrNames <- Rcpp_ShapeMap_getAttributeNames(from@ptr)
  result <- st_sf(
    Rcpp_ShapeMap_getAttributeData(from@ptr, attrNames),
    geometry = sfGeom
  )
  return(result[c(attrNames, "geometry")])
})
