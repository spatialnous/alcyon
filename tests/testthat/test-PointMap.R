# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

context("PointMap tests")

test_that("PointMap linking", {
  pointMap <- loadInteriorLinesAsPointMap()$pointMap
  linkData <- links(pointMap)
  expect_identical(colnames(linkData), c("from", "to"))
  expect_identical(dim(linkData), c(0L, 2L))

  linkCoords(pointMap, 1.74, 6.7, 5.05, 5.24)
  linkData <- links(pointMap)
  expect_identical(colnames(linkData), c("from", "to"))
  expect_identical(dim(linkData), c(1L, 2L))

  unlinkCoords(pointMap, 1.74, 6.7, 5.05, 5.24)
  linkData <- links(pointMap)
  expect_identical(colnames(linkData), c("from", "to"))
  expect_identical(dim(linkData), c(0L, 2L))

  linkRefs(pointMap, 1835056L, 7208971L)
  linkData <- links(pointMap)
  expect_identical(colnames(linkData), c("from", "to"))
  expect_identical(dim(linkData), c(1L, 2L))

  unlinkRefs(pointMap, 1835056L, 7208971L)
  linkData <- links(pointMap)
  expect_identical(colnames(linkData), c("from", "to"))
  expect_identical(dim(linkData), c(0L, 2L))
})

test_that("PointMap linking", {
  pointMap <- loadInteriorLinesAsPointMap()$pointMap
  connectionData <- connections(pointMap)
  expect_identical(colnames(connectionData), c("from", "to"))
  expect_identical(dim(connectionData), c(887448L, 2L))
})
