# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

context("Axial ShapeGraph tests")

test_that("ShapeGraph Links and Unlinks", {
    shapeGraph <- loadSmallAxialLinesAsAxialMap()$axialMap

    # link two non-crossing lines using coordinates
    shapeGraph <- linkCoords(
        shapeGraph,
        0982.8, -1620.3,
        1217.1, -1977.3
    )
    linkUnlinks <- links(shapeGraph)
    expect_identical(dim(linkUnlinks), c(1L, 3L))
    expect_identical(
        linkUnlinks,
        matrix(c(6.0, 11.0, 0.0),
            byrow = TRUE,
            ncol = 3L,
            dimnames = list(
                vector(mode = "character"),
                c("from", "to", "isunlink")
            )
        )
    )

    # unlink the two linked non-crossing lines using coordinates
    shapeGraph <- unlinkCoords(
        shapeGraph,
        0982.8, -1620.3,
        1217.1, -1977.3
    )
    linkUnlinks <- links(shapeGraph)
    expect_identical(dim(linkUnlinks), c(0L, 3L))

    # unlink two crossing lines using coordinates
    shapeGraph <- unlinkCoords(
        shapeGraph,
        0982.8, -1620.3,
        1080.4, -1873.5
    )
    linkUnlinks <- links(shapeGraph)
    expect_identical(dim(linkUnlinks), c(1L, 3L))
    expect_identical(
        linkUnlinks,
        matrix(c(6.0, 9.0, 1.0),
            byrow = TRUE,
            ncol = 3L,
            dimnames = list(
                vector(mode = "character"),
                c("from", "to", "isunlink")
            )
        )
    )

    # link the two unlinked crossing lines using coordinates
    shapeGraph <- linkCoords(
        shapeGraph,
        0982.8, -1620.3,
        1080.4, -1873.5
    )
    linkUnlinks <- links(shapeGraph)
    expect_identical(dim(linkUnlinks), c(0L, 3L))


    # link two non-crossing lines using refs
    shapeGraph <- linkRefs(shapeGraph, 6L, 11L)
    linkUnlinks <- links(shapeGraph)
    expect_identical(dim(linkUnlinks), c(1L, 3L))
    expect_identical(
        linkUnlinks,
        matrix(c(6.0, 11.0, 0.0),
            byrow = TRUE,
            ncol = 3L,
            dimnames = list(
                vector(mode = "character"),
                c("from", "to", "isunlink")
            )
        )
    )

    # unlink the two linked non-crossing lines using refs
    shapeGraph <- unlinkRefs(shapeGraph, 6L, 11L)
    linkUnlinks <- links(shapeGraph)
    expect_identical(dim(linkUnlinks), c(0L, 3L))

    # unlink two crossing lines using refs
    shapeGraph <- unlinkRefs(shapeGraph, 12L, 34L)
    linkUnlinks <- links(shapeGraph)
    expect_identical(dim(linkUnlinks), c(1L, 3L))
    expect_identical(
        linkUnlinks,
        matrix(c(12.0, 34.0, 1.0),
            byrow = TRUE,
            ncol = 3L,
            dimnames = list(
                vector(mode = "character"),
                c("from", "to", "isunlink")
            )
        )
    )

    # link the two unlinked crossing lines using refs
    shapeGraph <- linkRefs(shapeGraph, 12L, 34L)
    linkUnlinks <- links(shapeGraph)
    expect_identical(dim(linkUnlinks), c(0L, 3L))


    # unlink two crossing lines using coordinates of the crossing point
    shapeGraph <- unlinkAtCrossPoint(
        shapeGraph,
        1017.3, -1875.1
    )
    linkUnlinks <- links(shapeGraph)
    expect_identical(dim(linkUnlinks), c(1L, 3L))
    expect_identical(
        linkUnlinks,
        matrix(c(6.0, 9.0, 1.0),
            byrow = TRUE,
            ncol = 3L,
            dimnames = list(
                vector(mode = "character"),
                c("from", "to", "isunlink")
            )
        )
    )
})
