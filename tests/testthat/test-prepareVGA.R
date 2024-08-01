# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

context("PointMap tests")

test_that("PointMaps in C++", {
    startData <- loadInteriorLinesAsShapeMap(vector())
    lineStringMap <- startData$sf
    boundaryMap <- startData$shapeMap

    mapRegion <- sf::st_bbox(lineStringMap)

    pointMapPtr <- Rcpp_PointMap_createFromGrid(
        mapRegion[["xmin"]],
        mapRegion[["ymin"]],
        mapRegion[["xmax"]],
        mapRegion[["ymax"]],
        0.04
    )


    pointMapPtr <- Rcpp_PointMap_blockLines(
        pointMapPtr = pointMapPtr,
        boundaryMap = attr(boundaryMap, "sala_map")
    )$mapPtr

    pointMapPtr <- Rcpp_PointMap_fill(
        pointMapPtr = pointMapPtr,
        pointCoords = matrix(c(3.01, 6.7), nrow = 1L)
    )$mapPtr

    pointMapPtr <- Rcpp_PointMap_makeGraph(
        pointMapPtr = pointMapPtr,
        boundaryGraph = FALSE,
        maxVisibility = -1.0
    )$mapPtr

    coords <- Rcpp_PointMap_getFilledPoints(pointMapPtr = pointMapPtr)
    expect_identical(dim(coords), c(4332L, 10L))
    expect_identical(colnames(coords), c(
        "x", "y", "filled", "blocked",
        "contextfilled", "edge", "Ref",
        "Connectivity", "Point First Moment",
        "Point Second Moment"
    ))
})

test_that("PointMaps in R", {
    lineStringMap <- loadInteriorLinesAsSf()$sf

    pointMap <- makeVGAPointMap(
        lineStringMap,
        gridSize = 0.04,
        fillX = 3.01,
        fillY = 6.7,
        maxVisibility = NA,
        boundaryGraph = FALSE,
        verbose = FALSE
    )

    coords <- Rcpp_PointMap_getFilledPoints(
        pointMapPtr = attr(pointMap, "sala_map")
    )

    expect_identical(dim(coords), c(4332L, 10L))
    expect_identical(colnames(coords), c(
        "x", "y", "filled", "blocked",
        "contextfilled", "edge", "Ref",
        "Connectivity", "Point First Moment",
        "Point Second Moment"
    ))
})
