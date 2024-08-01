# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

loadTestDataMap <- function(folder, file, keepAttributeIdx = NULL) {
    sfMap <- st_read(
        system.file(
            "extdata", "testdata", folder,
            file,
            package = "alcyon"
        ),
        geometry_column = 1L, quiet = TRUE
    )
    if (!is.null(keepAttributeIdx)) {
        sfMap <- sfMap[keepAttributeIdx]
    }
    return(sfMap)
}

loadInteriorLinesAsSf <- function(keepAttributeIdx = NULL) {
    return(list(
        sf = loadTestDataMap(
            "gallery",
            "gallery_lines.mif",
            keepAttributeIdx
        )
    ))
}

loadInteriorLinesAsShapeMap <- function(keepAttributeIdx = NULL) {
    map <- loadInteriorLinesAsSf(keepAttributeIdx)
    map[["shapeMap"]] <- as(map$sf, "ShapeMap")
    return(map)
}

loadInteriorLinesAsPointMap <- function(keepAttributeIdx = NULL) {
    map <- loadInteriorLinesAsSf(keepAttributeIdx)
    map[["pointMap"]] <- makeVGAPointMap(
        map$sf,
        gridSize = 0.04,
        fillX = 3.01,
        fillY = 6.7,
        maxVisibility = NA,
        boundaryGraph = FALSE,
        verbose = FALSE
    )
    return(map)
}

loadSimpleLinesAsSf <- function(keepAttributeIdx = NULL) {
    return(list(
        sf = loadTestDataMap(
            "simple",
            "simple_interior.mif",
            keepAttributeIdx
        )
    ))
}

loadSimpleLinesAsShapeMap <- function(keepAttributeIdx = NULL) {
    map <- loadSimpleLinesAsSf(keepAttributeIdx)
    map[["shapeMap"]] <- as(map$sf, "ShapeMap")
    return(map)
}

loadSimpleLinesAsPointMap <- function(keepAttributeIdx = NULL) {
    map <- loadSimpleLinesAsSf(keepAttributeIdx)
    map[["pointMap"]] <- makeVGAPointMap(
        map$sf,
        gridSize = 0.5,
        fillX = 3.0,
        fillY = 6.0,
        maxVisibility = NA,
        boundaryGraph = FALSE,
        verbose = FALSE
    )
    return(map)
}

loadSmallAxialLinesAsSf <- function(keepAttributeIdx = NULL) {
    return(list(
        sf = loadTestDataMap(
            "barnsbury",
            "barnsbury_small_axial_original.mif",
            keepAttributeIdx
        )
    ))
}

loadSmallAxialLinesAsShapeMap <- function(keepAttributeIdx = NULL) {
    map <- loadSmallAxialLinesAsSf(keepAttributeIdx)
    map[["shapeMap"]] <- as(map$sf, "ShapeMap")
    return(map)
}

loadSmallAxialLinesAsAxialMap <- function(keepAttributeIdx = NULL) {
    map <- loadSmallAxialLinesAsSf(keepAttributeIdx)
    map[["axialMap"]] <- as(map$sf, "AxialShapeGraph")
    return(map)
}

loadSmallAxialLinesAsSegmMap <- function(keepAttributeIdx = NULL) {
    map <- loadSmallAxialLinesAsAxialMap(keepAttributeIdx)
    map[["segmentMap"]] <- axialToSegmentShapeGraph(
        map$axialMap,
        stubRemoval = 0.4
    )
    return(map)
}

loadSmallSegmentLinesAsSf <- function(keepAttributeIdx = NULL) {
    return(list(
        sf = loadTestDataMap(
            "barnsbury",
            "barnsbury_small_segment_original.mif",
            keepAttributeIdx
        )
    ))
}

loadSmallSegmLinesAsSegmMap <- function(keepAttributeIdx = NULL) {
    map <- loadSmallSegmentLinesAsSf(keepAttributeIdx)
    map[["segmentMap"]] <- as(map$sf, "SegmentShapeGraph")
    return(map)
}

loadInteriorPolygonsAsSf <- function(keepAttributeIdx = NULL) {
    return(list(
        sf = loadTestDataMap(
            "gallery",
            "gallery_polys.mif",
            keepAttributeIdx
        )
    ))
}
