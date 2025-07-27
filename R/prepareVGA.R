# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

#' Create a LatticeMap through a grid
#'
#' @param minX Minimum X of the bounding region
#' @param minY Minimum Y of the bounding region
#' @param maxX Maximum X of the bounding region
#' @param maxY Maximum Y of the bounding region
#' @param gridSize Size of the cells
#' @param verbose Optional. Show more information of the process.
#' @returns A new LatticeMap
#' @importFrom stars st_as_stars
#' @eval c("@examples",
#' rxLoadSimpleLinesAsShapeMap(),
#' "lineStringMap <- as(sfMap, \"sf\")",
#' "mapRegion <- sf::st_bbox(lineStringMap)",
#' "createGrid(",
#' "  minX = mapRegion[[\"xmin\"]],",
#' "  minY = mapRegion[[\"ymin\"]],",
#' "  maxX = mapRegion[[\"xmax\"]],",
#' "  maxY = mapRegion[[\"ymax\"]],",
#' "  gridSize = 0.04",
#' ")")
#' @export
createGrid <- function(minX,
                       minY,
                       maxX,
                       maxY,
                       gridSize,
                       verbose = FALSE) {
    latticeMapPtr <- Rcpp_LatticeMap_createFromGrid(
        minX,
        minY,
        maxX,
        maxY,
        gridSize
    )
    coordData <- Rcpp_LatticeMap_getGridCoordinates(latticeMapPtr)
    starsObj <- st_as_stars(as.data.frame(coordData))
    attr(starsObj, "sala_map") <- latticeMapPtr
    class(starsObj) <- c("LatticeMap", class(starsObj))
    return(starsObj)
}

#' Block lines on a LatticeMap
#'
#' Takes a LatticeMap and a ShapeMap with lines and blocks the cells on the
#' LatticeMap where the lines pass.
#'
#' @param latticeMap The input LatticeMap
#' @param lineStringMap Map of lines, either a ShapeMap, or an sf lineString map
#' @param copyMap Optional. Copy the internal sala map
#' @param verbose Optional. Show more information of the process.
#' @returns A new LatticeMap with points as they have been blocked by the lines
#' @eval c("@examples",
#' rxLoadSimpleLinesAsShapeMap(),
#' "lineStringMap <- as(sfMap, \"sf\")",
#' "mapRegion <- sf::st_bbox(lineStringMap)",
#' "latticeMap <- createGrid(",
#' "  minX = mapRegion[[\"xmin\"]],",
#' "  minY = mapRegion[[\"ymin\"]],",
#' "  maxX = mapRegion[[\"xmax\"]],",
#' "  maxY = mapRegion[[\"ymax\"]],",
#' "  gridSize = 0.04",
#' ")",
#' "blockLines(",
#' "  latticeMap = latticeMap,",
#' "  lineStringMap = lineStringMap[vector()]",
#' ")")
#' @export
blockLines <- function(latticeMap,
                       lineStringMap,
                       copyMap = TRUE,
                       verbose = FALSE) {
    boundaryMap <- lineStringMap
    if (!inherits(lineStringMap, "ShapeMap")) {
        boundaryMap <- as(lineStringMap, "ShapeMap")
    }
    result <- Rcpp_LatticeMap_blockLines(
        latticeMapPtr = attr(latticeMap, "sala_map"),
        boundaryMapPtr = attr(boundaryMap, "sala_map"),
        copyMapNV = copyMap
    )
    return(processLatticeMapResult(latticeMap, result))
}

#' Fill a LatticeMap's grid starting from one or more points
#'
#' @param latticeMap The input LatticeMap
#' @param fillX X coordinate of the fill points
#' @param fillY Y coordinate of the fill points
#' @param copyMap Optional. Copy the internal sala map
#' @param verbose Optional. Show more information of the process.
#' @returns A new LatticeMap with filled points
#' @eval c("@examples",
#' rxLoadSimpleLinesAsShapeMap(),
#' "lineStringMap <- as(sfMap, \"sf\")",
#' "mapRegion <- sf::st_bbox(lineStringMap)",
#' "latticeMap <- createGrid(",
#' "  minX = mapRegion[[\"xmin\"]],",
#' "  minY = mapRegion[[\"ymin\"]],",
#' "  maxX = mapRegion[[\"xmax\"]],",
#' "  maxY = mapRegion[[\"ymax\"]],",
#' "  gridSize = 0.04",
#' ")",
#' "latticeMap <- blockLines(",
#' "  latticeMap = latticeMap,",
#' "  lineStringMap = lineStringMap[vector()]",
#' ")",
#' "fillGrid(",
#' "  latticeMap = latticeMap,",
#' "  fillX = 3.01,",
#' "  fillY = 6.7",
#' ")")
#' @export
fillGrid <- function(latticeMap,
                     fillX,
                     fillY,
                     copyMap = TRUE,
                     verbose = FALSE) {
    result <- Rcpp_LatticeMap_fill(
        latticeMapPtr = attr(latticeMap, "sala_map"),
        pointCoords = cbind(fillX, fillY),
        copyMapNV = copyMap
    )
    return(processLatticeMapResult(latticeMap, result))
}

#' Create a graph between visible cells in the LatticeMap
#'
#' @param latticeMap The input LatticeMap
#' @param boundaryGraph Only create a graph on the boundary cells
#' @param maxVisibility Limit how far two cells can be to be connected
#' @param copyMap Optional. Copy the internal sala map
#' @param verbose Optional. Show more information of the process.
#' @returns A new LatticeMap with a graph between points
#' @eval c("@examples",
#' rxLoadSimpleLinesAsShapeMap(),
#' "lineStringMap <- as(sfMap, \"sf\")",
#' "mapRegion <- sf::st_bbox(lineStringMap)",
#' "latticeMap <- createGrid(",
#' "  minX = mapRegion[[\"xmin\"]],",
#' "  minY = mapRegion[[\"ymin\"]],",
#' "  maxX = mapRegion[[\"xmax\"]],",
#' "  maxY = mapRegion[[\"ymax\"]],",
#' "  gridSize = 0.5",
#' ")",
#' "latticeMap <- blockLines(",
#' "  latticeMap = latticeMap,",
#' "  lineStringMap = lineStringMap[vector()]",
#' ")",
#' "latticeMap <- fillGrid(",
#' "  latticeMap = latticeMap,",
#' "  fillX = 3.01,",
#' "  fillY = 6.7",
#' ")",
#' "makeVGAGraph(",
#' "  latticeMap = latticeMap,",
#' "  boundaryGraph = FALSE,",
#' "  maxVisibility = NA",
#' ")")
#' @export
makeVGAGraph <- function(latticeMap,
                         boundaryGraph = FALSE,
                         maxVisibility = NA,
                         copyMap = TRUE,
                         verbose = FALSE) {
    result <- Rcpp_LatticeMap_makeGraph(
        latticeMapPtr = attr(latticeMap, "sala_map"),
        boundaryGraph = boundaryGraph,
        maxVisibility = maxVisibility,
        copyMapNV = copyMap
    )
    return(processLatticeMapResult(latticeMap, result))
}

#' Create a LatticeMap grid, fill it and make the graph
#'
#' This is intended to be a single command to get from the lines to a LatticeMap
#' ready for analysis
#'
#' @param lineStringMap Map of lines, either a ShapeMap, or an sf lineString map
#' @param gridSize Size of the cells
#' @param fillX X coordinate of the fill points
#' @param fillY Y coordinate of the fill points
#' @param boundaryGraph Only create a graph on the boundary cells
#' @param maxVisibility Limit how far two cells can be to be connected
#' @param verbose Optional. Show more information of the process.
#' @returns A new LatticeMap
#' @eval c("@examples",
#' rxLoadSimpleLinesAsShapeMap(),
#' "makeVGALatticeMap(",
#' "  sfMap,",
#' "  gridSize = 0.5,",
#' "  fillX = 3.01,",
#' "  fillY = 6.7,",
#' "  maxVisibility = NA,",
#' "  boundaryGraph = FALSE,",
#' "  verbose = FALSE",
#' ")")
#' @export
makeVGALatticeMap <- function(lineStringMap,
                              gridSize,
                              fillX,
                              fillY,
                              maxVisibility = NA,
                              boundaryGraph = FALSE,
                              verbose = FALSE) {
    mapRegion <- sf::st_bbox(lineStringMap)

    latticeMap <- createGrid(
        mapRegion[["xmin"]],
        mapRegion[["ymin"]],
        mapRegion[["xmax"]],
        mapRegion[["ymax"]],
        gridSize
    )

    boundaryMap <- lineStringMap[, vector()]
    if (!inherits(lineStringMap, "ShapeMap")) {
        boundaryMap <- as(lineStringMap[, vector()], "ShapeMap")
    }
    finalResult <- Rcpp_LatticeMap_blockLines(
        latticeMapPtr = attr(latticeMap, "sala_map"),
        boundaryMapPtr = attr(boundaryMap, "sala_map"),
        copyMapNV = FALSE
    )

    result <- Rcpp_LatticeMap_fill(
        latticeMapPtr = attr(latticeMap, "sala_map"),
        pointCoords = cbind(fillX, fillY),
        copyMapNV = FALSE
    )

    finalResult$newAttributes <- c(
        finalResult$newAttributes,
        result$newAttributes
    )
    finalResult$newProperties <- c(
        finalResult$newProperties,
        result$newProperties
    )
    finalResult$completed <- finalResult$completed && result$completed

    result <- Rcpp_LatticeMap_makeGraph(
        latticeMapPtr = attr(latticeMap, "sala_map"),
        boundaryGraph = boundaryGraph,
        maxVisibility = maxVisibility,
        copyMapNV = FALSE
    )

    finalResult$newAttributes <- c(
        finalResult$newAttributes,
        result$newAttributes
    )
    finalResult$newProperties <- c(
        finalResult$newProperties,
        result$newProperties
    )
    finalResult$completed <- finalResult$completed && result$completed

    return(processLatticeMapResult(latticeMap, finalResult))
}

#' Unmake the graph in a LatticeMap
#'
#' @param latticeMap The input LatticeMap
#' @param removeLinks Also remove the links
#' @param copyMap Optional. Copy the internal sala map
#' @param verbose Optional. Show more information of the process.
#' @returns A new LatticeMap without the points graph
#' @eval c("@examples",
#' rxLoadSimpleLinesAsShapeMap(),
#' "latticeMap <- makeVGALatticeMap(",
#' "  sfMap,",
#' "  gridSize = 0.5,",
#' "  fillX = 3.01,",
#' "  fillY = 6.7,",
#' "  maxVisibility = NA,",
#' "  boundaryGraph = FALSE,",
#' "  verbose = FALSE",
#' ")",
#' "unmakeVGAGraph(",
#' "  latticeMap = latticeMap,",
#' "  removeLinks = FALSE",
#' ")")
#' @export
unmakeVGAGraph <- function(latticeMap,
                           removeLinks = FALSE,
                           copyMap = TRUE,
                           verbose = FALSE) {
    result <- Rcpp_LatticeMap_unmakeGraph(
        latticeMapPtr = attr(latticeMap, "sala_map"),
        removeLinksWhenUnmaking = removeLinks,
        copyMapNV = copyMap
    )
    return(processLatticeMapResult(latticeMap, result))
}
