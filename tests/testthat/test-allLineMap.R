# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

context("All-line Map tests")

test_that("All-line Map in C++", {
  boundsMap <- st_read(
    system.file(
      "extdata", "testdata", "gallery",
      "gallery_lines.mif",
      package = "alcyon"
    ),
    geometry_column = 1L, quiet = TRUE
  )

  shapeGraph <- sfToShapeMap(
    boundsMap,
    keepAttributes = vector(mode = "integer")
  )

  allLineMap <- Rcpp_makeAllLineMap(
    shapeGraph@ptr,
    seedX = 3.01,
    seedY = 6.7
  )

  fewestMaps <- Rcpp_extractFewestLineMaps(allLineMap)

  fewestSubsets <- fewestMaps[["Fewest-Line Map (Subsets)"]]

  attrNames <- Rcpp_ShapeMap_getAttributeNames(allLineMap)
  expect_length(attrNames, 3L)

  attrData <- Rcpp_ShapeMap_getAttributeData(allLineMap, attrNames)
  expect_length(attrData[[attrNames[[1L]]]], 1874L)
  expect_length(attrData[[attrNames[[2L]]]], 1874L)
  expect_length(attrData[[attrNames[[3L]]]], 1874L)

  coords <- Rcpp_ShapeMap_getShapesAsLineCoords(allLineMap)
  expect_identical(dim(coords), c(1874L, 4L))


  attrNames <- Rcpp_ShapeMap_getAttributeNames(fewestSubsets)
  expect_length(attrNames, 3L)

  attrData <- Rcpp_ShapeMap_getAttributeData(fewestSubsets, attrNames)
  expect_length(attrData[[attrNames[[1L]]]], 46L)
  expect_length(attrData[[attrNames[[2L]]]], 46L)
  expect_length(attrData[[attrNames[[3L]]]], 46L)

  coords <- Rcpp_ShapeMap_getShapesAsLineCoords(fewestSubsets)
  expect_identical(dim(coords), c(46L, 4L))


  fewestMinimal <- fewestMaps[["Fewest-Line Map (Minimal)"]]

  attrNames <- Rcpp_ShapeMap_getAttributeNames(fewestMinimal)
  expect_length(attrNames, 3L)

  attrData <- Rcpp_ShapeMap_getAttributeData(fewestMinimal, attrNames)
  expect_length(attrData[[attrNames[[1L]]]], 26L)
  expect_length(attrData[[attrNames[[2L]]]], 26L)
  expect_length(attrData[[attrNames[[3L]]]], 26L)

  coords <- Rcpp_ShapeMap_getShapesAsLineCoords(fewestMinimal)
  expect_identical(dim(coords), c(26L, 4L))
})

test_that("All-line Map in R", {
  boundsMap <- st_read(
    system.file(
      "extdata", "testdata", "gallery",
      "gallery_lines.mif",
      package = "alcyon"
    ),
    geometry_column = 1L, quiet = TRUE
  )

  shapeMap <- sfToShapeMap(
    boundsMap,
    keepAttributes = vector(mode = "integer")
  )
  result <- allFewestLineMap(
    shapeMap,
    seedX = 3.01,
    seedY = 6.7,
    calculateFewest = TRUE
  )

  allLineMap <- result[["All Line Map"]]
  fewestSubsets <- result[["Fewest-Line Map (Subsets)"]]
  fewestMinimal <- result[["Fewest-Line Map (Minimal)"]]

  # All line

  expect_length(colnames(allLineMap), 4L)
  expect_identical(nrow(allLineMap), 1874L)

  # Fewest (Subsets)

  expect_length(colnames(fewestSubsets), 4L)
  expect_identical(nrow(fewestSubsets), 46L)

  # Fewest (Minimal)

  expect_length(colnames(fewestMinimal), 4L)
  expect_identical(nrow(fewestMinimal), 26L)
})
