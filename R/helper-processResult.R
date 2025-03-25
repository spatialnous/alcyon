# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

processShapeMapResult <- function(shapeGraph, result) {
    if (!result$completed) stop("Analysis did not complete", call. = FALSE)
    if (length(result$newAttributes) != 0L) {
        newAttrs <- Rcpp_ShapeMap_getAttributeData(
            result$mapPtr,
            result$newAttributes
        )
        for (newAttr in names(newAttrs)) {
            shapeGraph[newAttr] <- newAttrs[[newAttr]]
        }
    }
    attr(shapeGraph, "sala_map") <- result$mapPtr
    return(shapeGraph)
}

processPointMapResult <- function(pointMap, result) {
    if (!result$completed) stop("Analysis did not complete", call. = FALSE)
    if (length(result$newAttributes) != 0L) {
        newAttrs <- Rcpp_PointMap_getAttributeData(
            result$mapPtr,
            result$newAttributes
        )
        for (newAttr in names(newAttrs)) {
            pointMap[newAttr] <- newAttrs[[newAttr]]
        }
    }
    if (length(result$newProperties) != 0L) {
        newProps <- Rcpp_PointMap_getPropertyData(
            result$mapPtr,
            result$newProperties
        )
        for (newProp in names(newProps)) {
            pointMap[newProp] <- newProps[[newProp]]
        }
    }
    attr(pointMap, "sala_map") <- result$mapPtr
    return(pointMap)
}

processPtrAsNewPointMap <- function(pointMapPtr) {
    coordData <- Rcpp_PointMap_getFilledPoints(pointMapPtr)
    starsObj <- st_as_stars(as.data.frame(coordData))
    attr(starsObj, "sala_map") <- pointMapPtr
    class(starsObj) <- c("PointMap", class(starsObj))
    return(starsObj)
}

processPtrAsNewLineMap <- function(shapeMapPtr, newClasses) {
    coords <- Rcpp_ShapeMap_getShapesAsLineCoords(shapeMapPtr)
    sfGeom <- st_sfc(lapply(seq_len(nrow(coords)), function(rowIdx) {
        sf::st_linestring(
            matrix(coords[rowIdx, ], ncol = 2L, byrow = TRUE),
            dim = "XY"
        )
    }))
    attrNames <- Rcpp_ShapeMap_getAttributeNames(shapeMapPtr)
    newSf <- st_sf(
        Rcpp_ShapeMap_getAttributeData(shapeMapPtr, attrNames),
        geometry = sfGeom
    )
    attr(newSf, "sala_map") <- shapeMapPtr
    class(newSf) <- c(newClasses, class(newSf))
    return(newSf)
}

processPtrAsNewPolyMap <- function(shapeMapPtr, newClasses) {
    coords <- Rcpp_ShapeMap_getShapesAsPolygonCoords(shapeMapPtr)
    sfGeom <- st_sfc(lapply(coords, function(polyCoords) {
        sf::st_polygon(list(polyCoords), dim = "XY")
    }))
    attrNames <- Rcpp_ShapeMap_getAttributeNames(shapeMapPtr)
    newSf <- st_sf(
        Rcpp_ShapeMap_getAttributeData(shapeMapPtr, attrNames),
        geometry = sfGeom
    )
    attr(newSf, "sala_map") <- shapeMapPtr
    class(newSf) <- c(newClasses, class(newSf))
    return(newSf)
}

processPtrAsNewPolylineMap <- function(shapeMapPtr, newClasses) {
    coords <- Rcpp_ShapeMap_getShapesAsPolylineCoords(shapeMapPtr)
    sfGeom <- st_sfc(lapply(coords, sf::st_linestring, dim = "XY"))
    attrNames <- Rcpp_ShapeMap_getAttributeNames(shapeMapPtr)
    newSf <- st_sf(
        Rcpp_ShapeMap_getAttributeData(shapeMapPtr, attrNames),
        geometry = sfGeom
    )
    attr(newSf, "sala_map") <- shapeMapPtr
    class(newSf) <- c(newClasses, class(newSf))
    return(newSf)
}
