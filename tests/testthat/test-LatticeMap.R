# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

context("LatticeMap tests")

test_that("LatticeMap linking", {
    latticeMap <- loadInteriorLinesAsLatticeMap()$latticeMap
    linkData <- links(latticeMap)
    expect_identical(colnames(linkData), c("from", "to"))
    expect_identical(dim(linkData), c(0L, 2L))

    latticeMap <- linkCoords(latticeMap, 1.74, 6.7, 5.05, 5.24)
    linkData <- links(latticeMap)
    expect_identical(colnames(linkData), c("from", "to"))
    expect_identical(dim(linkData), c(1L, 2L))

    latticeMap <- unlinkCoords(latticeMap, 1.74, 6.7, 5.05, 5.24)
    linkData <- links(latticeMap)
    expect_identical(colnames(linkData), c("from", "to"))
    expect_identical(dim(linkData), c(0L, 2L))

    latticeMap <- linkRefs(latticeMap, 1835056L, 7208971L)
    linkData <- links(latticeMap)
    expect_identical(colnames(linkData), c("from", "to"))
    expect_identical(dim(linkData), c(1L, 2L))

    latticeMap <- unlinkRefs(latticeMap, 1835056L, 7208971L)
    linkData <- links(latticeMap)
    expect_identical(colnames(linkData), c("from", "to"))
    expect_identical(dim(linkData), c(0L, 2L))
})

test_that("LatticeMap linking", {
    latticeMap <- loadInteriorLinesAsLatticeMap()$latticeMap
    connectionData <- connections(latticeMap)
    expect_identical(colnames(connectionData), c("from", "to"))
    expect_identical(dim(connectionData), c(887448L, 2L))
})
