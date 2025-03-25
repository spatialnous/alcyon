# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

#' ShapeMap class
#'
#' A representation of sala's ShapeMap in R. Holds onto a sala ShapeMap
#' pointer and operates on that
#' @name ShapeMap-class
#' @aliases ShapeMap
#' @family ShapeMap
#' @importFrom methods setOldClass
#' @exportClass ShapeMap
setOldClass(c("ShapeMap", "sf"))

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
        Rcpp_ShapeMap_getName(attr(map, "sala_map"))
    }
)

#' as("sf", "ShapeMap")
#'
#' @name as
#' @family ShapeMap
#'
#' @importFrom methods as S3Part<-
setAs("sf", "ShapeMap", function(from) {
    shapeMap <- from
    class(shapeMap) <- c("ShapeMap", class(shapeMap))
    cols <- names(from)[names(from) != "geometry"]
    if (length(cols) != 0L) {
        numericCols <- unlist(lapply(cols, function(col) {
            is.numeric(from[[col]])
        }))
        if (!all(numericCols)) {
            warning(
                "Non-numeric columns will not be transferred to ",
                "the ShapeMap: ",
                do.call(paste, as.list(cols[!numericCols])),
                call. = FALSE
            )
        }
        attr(shapeMap, "sala_map") <- Rcpp_toShapeMap(from, which(numericCols))
    } else {
        attr(shapeMap, "sala_map") <- Rcpp_toShapeMap(from)
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
    fromPtr <- attr(from, "sala_map")
    coords <- Rcpp_ShapeMap_getShapesAsLineCoords(fromPtr)
    sfGeom <- st_sfc(lapply(seq_len(nrow(coords)), function(rowIdx) {
        sf::st_linestring(
            matrix(coords[rowIdx, ], ncol = 2L, byrow = TRUE),
            dim = "XY"
        )
    }))
    attrNames <- Rcpp_ShapeMap_getAttributeNames(fromPtr)
    result <- st_sf(
        Rcpp_ShapeMap_getAttributeData(fromPtr, attrNames),
        geometry = sfGeom
    )
    return(result[c(attrNames, "geometry")])
})

#' Subset ShapeMap objects
#'
#' Subsetting ShapeMap objects essentially passes the data to sf.
#' See \link[sf]{sf}
#'
#' @name ShapeMap_subset
#' @param x object of class \code{ShapeMap} passed to \code{sf[]}
#' @param ... other parameters passed to \code{sf[]}
#' @export
`[.ShapeMap` <- function(x, ...) {
    class(x) <- setdiff(class(x), "ShapeMap")
    x <- NextMethod()
    class(x) <- c("ShapeMap", class(x))
    return(x)
}

#' @name ShapeMap_subset
#' @param x object of class \code{ShapeMap} passed to \code{sf[]}
#' @param ... other parameters passed to \code{sf[] <- }
#' @param value value to be passed to \code{sf[] <- }
#' @export
`[<-.ShapeMap` <- function(x, ..., value) {
    class(x) <- setdiff(class(x), "ShapeMap")
    x <- NextMethod()
    class(x) <- c("ShapeMap", class(x))
    return(x)
}
