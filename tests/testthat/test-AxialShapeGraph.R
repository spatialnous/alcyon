# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

context("Axial ShapeGraph tests")

test_that("ShapeGraph Links/Unlinks", {
  shapeGraph <- loadSmallAxialLinesAsAxialMap()$axialMap

  # link two non-crossing lines using coordinates
  linkCoords(shapeGraph,
             530684, 184100.3,
             530807.5, 183969.3)
  linkUnlinks <- links(shapeGraph)
  expect_identical(dim(linkUnlinks), c(1L, 3L))
  expect_identical(
    linkUnlinks,
    matrix(c(0, 9, 0),
           byrow = T,
           ncol = 3,
           dimnames = list(c(), c("from", "to", "isunlink"))))

  # unlink the two linked non-crossing lines using coordinates
  unlinkCoords(shapeGraph,
               530684, 184100.3,
               530807.5, 183969.3)
  linkUnlinks <- links(shapeGraph)
  expect_identical(dim(linkUnlinks), c(0L, 3L))

  # unlink two crossing lines using coordinates
  unlinkCoords(shapeGraph,
               530923.0, 184041.0,
               530956.0, 183887.0)
  linkUnlinks <- links(shapeGraph)
  expect_identical(dim(linkUnlinks), c(1L, 3L))
  expect_identical(
    linkUnlinks,
    matrix(c(12, 34, 1),
           byrow = T,
           ncol = 3,
           dimnames = list(c(), c("from", "to", "isunlink"))))

  # link the two unlinked crossing lines using coordinates
  linkCoords(shapeGraph,
             530923.0, 184041.0,
             530956.0, 183887.0)
  linkUnlinks <- links(shapeGraph)
  expect_identical(dim(linkUnlinks), c(0L, 3L))


  # link two non-crossing lines using refs
  linkRefs(shapeGraph, 0, 9)
  linkUnlinks <- links(shapeGraph)
  expect_identical(dim(linkUnlinks), c(1L, 3L))
  expect_identical(
    linkUnlinks,
    matrix(c(0, 9, 0),
           byrow = T,
           ncol = 3,
           dimnames = list(c(), c("from", "to", "isunlink"))))

  # unlink the two linked non-crossing lines using refs
  unlinkRefs(shapeGraph, 0, 9)
  linkUnlinks <- links(shapeGraph)
  expect_identical(dim(linkUnlinks), c(0L, 3L))

  # unlink two crossing lines using refs
  unlinkRefs(shapeGraph, 12, 34)
  linkUnlinks <- links(shapeGraph)
  expect_identical(dim(linkUnlinks), c(1L, 3L))
  expect_identical(
    linkUnlinks,
    matrix(c(12, 34, 1),
           byrow = T,
           ncol = 3,
           dimnames = list(c(), c("from", "to", "isunlink"))))

  # link the two unlinked crossing lines using refs
  linkRefs(shapeGraph, 12, 34)
  linkUnlinks <- links(shapeGraph)
  expect_identical(dim(linkUnlinks), c(0L, 3L))


  # unlink two crossing lines using coordinates of the crossing point
  unlinkAtCrossPoint(shapeGraph,
                     530925.0, 184119.0)
  linkUnlinks <- links(shapeGraph)
  expect_identical(dim(linkUnlinks), c(1L, 3L))
  expect_identical(
    linkUnlinks,
    matrix(c(7, 12, 1),
           byrow = T,
           ncol = 3,
           dimnames = list(c(), c("from", "to", "isunlink"))))
})
