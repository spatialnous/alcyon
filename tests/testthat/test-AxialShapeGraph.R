# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

context("Axial ShapeGraph tests")

test_that("ShapeGraph Links and Unlinks", {
  shapeGraph <- loadSmallAxialLinesAsAxialMap()$axialMap

  # link two non-crossing lines using coordinates
  linkCoords(
    shapeGraph,
    530684.0, 184100.3,
    530807.5, 183969.3
  )
  linkUnlinks <- links(shapeGraph)
  expect_identical(dim(linkUnlinks), c(1L, 3L))
  expect_identical(
    linkUnlinks,
    matrix(c(0.0, 9.0, 0.0),
      byrow = TRUE,
      ncol = 3L,
      dimnames = list(vector(mode = "character"),
                      c("from", "to", "isunlink"))
    )
  )

  # unlink the two linked non-crossing lines using coordinates
  unlinkCoords(
    shapeGraph,
    530684.0, 184100.3,
    530807.5, 183969.3
  )
  linkUnlinks <- links(shapeGraph)
  expect_identical(dim(linkUnlinks), c(0L, 3L))

  # unlink two crossing lines using coordinates
  unlinkCoords(
    shapeGraph,
    530923.0, 184041.0,
    530956.0, 183887.0
  )
  linkUnlinks <- links(shapeGraph)
  expect_identical(dim(linkUnlinks), c(1L, 3L))
  expect_identical(
    linkUnlinks,
    matrix(c(12.0, 34.0, 1.0),
      byrow = TRUE,
      ncol = 3L,
      dimnames = list(vector(mode = "character"),
                      c("from", "to", "isunlink"))
    )
  )

  # link the two unlinked crossing lines using coordinates
  linkCoords(
    shapeGraph,
    530923.0, 184041.0,
    530956.0, 183887.0
  )
  linkUnlinks <- links(shapeGraph)
  expect_identical(dim(linkUnlinks), c(0L, 3L))


  # link two non-crossing lines using refs
  linkRefs(shapeGraph, 0L, 9L)
  linkUnlinks <- links(shapeGraph)
  expect_identical(dim(linkUnlinks), c(1L, 3L))
  expect_identical(
    linkUnlinks,
    matrix(c(0.0, 9.0, 0.0),
      byrow = TRUE,
      ncol = 3L,
      dimnames = list(vector(mode = "character"),
                      c("from", "to", "isunlink"))
    )
  )

  # unlink the two linked non-crossing lines using refs
  unlinkRefs(shapeGraph, 0L, 9L)
  linkUnlinks <- links(shapeGraph)
  expect_identical(dim(linkUnlinks), c(0L, 3L))

  # unlink two crossing lines using refs
  unlinkRefs(shapeGraph, 12L, 34L)
  linkUnlinks <- links(shapeGraph)
  expect_identical(dim(linkUnlinks), c(1L, 3L))
  expect_identical(
    linkUnlinks,
    matrix(c(12.0, 34.0, 1.0),
      byrow = TRUE,
      ncol = 3L,
      dimnames = list(vector(mode = "character"),
                      c("from", "to", "isunlink"))
    )
  )

  # link the two unlinked crossing lines using refs
  linkRefs(shapeGraph, 12L, 34L)
  linkUnlinks <- links(shapeGraph)
  expect_identical(dim(linkUnlinks), c(0L, 3L))


  # unlink two crossing lines using coordinates of the crossing point
  unlinkAtCrossPoint(
    shapeGraph,
    530925.0, 184119.0
  )
  linkUnlinks <- links(shapeGraph)
  expect_identical(dim(linkUnlinks), c(1L, 3L))
  expect_identical(
    linkUnlinks,
    matrix(c(7.0, 12.0, 1.0),
      byrow = TRUE,
      ncol = 3L,
      dimnames = list(vector(mode = "character"),
                      c("from", "to", "isunlink"))
    )
  )
})
