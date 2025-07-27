# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

context("LatticeMap tests")

test_that("LatticeMaps in C++", {
    startData <- loadInteriorLinesAsShapeMap(vector())
    lineStringMap <- startData$sf
    boundaryMap <- startData$shapeMap

    mapRegion <- sf::st_bbox(lineStringMap)

    latticeMapPtr <- Rcpp_LatticeMap_createFromGrid(
        mapRegion[["xmin"]],
        mapRegion[["ymin"]],
        mapRegion[["xmax"]],
        mapRegion[["ymax"]],
        0.04
    )


    latticeMapPtr <- Rcpp_LatticeMap_blockLines(
        latticeMapPtr = latticeMapPtr,
        boundaryMap = attr(boundaryMap, "sala_map")
    )$mapPtr

    latticeMapPtr <- Rcpp_LatticeMap_fill(
        latticeMapPtr = latticeMapPtr,
        pointCoords = matrix(c(3.01, 6.7), nrow = 1L)
    )$mapPtr

    latticeMapPtr <- Rcpp_LatticeMap_makeGraph(
        latticeMapPtr = latticeMapPtr,
        boundaryGraph = FALSE,
        maxVisibility = -1.0
    )$mapPtr

    coords <- Rcpp_LatticeMap_getFilledPoints(latticeMapPtr = latticeMapPtr)
    expect_identical(dim(coords), c(4332L, 10L))
    expect_identical(colnames(coords), c(
        "x", "y", "filled", "blocked",
        "contextfilled", "edge", "Ref",
        "Connectivity", "Point First Moment",
        "Point Second Moment"
    ))
})

test_that("LatticeMaps in R", {
    lineStringMap <- loadInteriorLinesAsSf()$sf

    latticeMap <- makeVGALatticeMap(
        lineStringMap,
        gridSize = 0.04,
        fillX = 3.01,
        fillY = 6.7,
        maxVisibility = NA,
        boundaryGraph = FALSE,
        verbose = FALSE
    )

    coords <- Rcpp_LatticeMap_getFilledPoints(
        latticeMapPtr = attr(latticeMap, "sala_map")
    )

    expect_identical(dim(coords), c(4332L, 10L))
    expect_identical(colnames(coords), c(
        "x", "y", "filled", "blocked",
        "contextfilled", "edge", "Ref",
        "Connectivity", "Point First Moment",
        "Point Second Moment"
    ))
})
