# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

context("All-line Map tests")

test_that("All-line Map in C++", {
  shapeMap <- loadSimpleLinesAsShapeMap(vector())$shapeMap

  allLineMap <- Rcpp_makeAllLineMap(
    attr(shapeMap, "sala_map"),
    seedX = 3.01,
    seedY = 6.7
  )

  fewestMaps <- Rcpp_extractFewestLineMaps(allLineMap$allLineMap,
                                           allLineMap$mapData)

  fewestSubsets <- fewestMaps[["Fewest-Line Map (Subsets)"]]

  attrNames <- Rcpp_ShapeMap_getAttributeNames(allLineMap$allLineMap)
  expect_length(attrNames, 3L)

  attrData <- Rcpp_ShapeMap_getAttributeData(allLineMap$allLineMap, attrNames)
  expect_length(attrData[[attrNames[[1L]]]], 7L)
  expect_length(attrData[[attrNames[[2L]]]], 7L)
  expect_length(attrData[[attrNames[[3L]]]], 7L)

  coords <- Rcpp_ShapeMap_getShapesAsLineCoords(allLineMap$allLineMap)
  expect_identical(dim(coords), c(7L, 4L))


  attrNames <- Rcpp_ShapeMap_getAttributeNames(fewestSubsets)
  expect_length(attrNames, 3L)

  attrData <- Rcpp_ShapeMap_getAttributeData(fewestSubsets, attrNames)
  expect_length(attrData[[attrNames[[1L]]]], 1L)
  expect_length(attrData[[attrNames[[2L]]]], 1L)
  expect_length(attrData[[attrNames[[3L]]]], 1L)

  coords <- Rcpp_ShapeMap_getShapesAsLineCoords(fewestSubsets)
  expect_identical(dim(coords), c(1L, 4L))


  fewestMinimal <- fewestMaps[["Fewest-Line Map (Minimal)"]]

  attrNames <- Rcpp_ShapeMap_getAttributeNames(fewestMinimal)
  expect_length(attrNames, 3L)

  attrData <- Rcpp_ShapeMap_getAttributeData(fewestMinimal, attrNames)
  expect_length(attrData[[attrNames[[1L]]]], 1L)
  expect_length(attrData[[attrNames[[2L]]]], 1L)
  expect_length(attrData[[attrNames[[3L]]]], 1L)

  coords <- Rcpp_ShapeMap_getShapesAsLineCoords(fewestMinimal)
  expect_identical(dim(coords), c(1L, 4L))
})

test_that("All-line Map in R", {
  shapeMap <- loadSimpleLinesAsShapeMap(vector())$shapeMap

  allLineMap <- makeAllLineMap(
    shapeMap,
    seedX = 3.01,
    seedY = 6.7
  )
  allLineMapSf <- as(allLineMap, "sf")

  fewestMaps <- reduceToFewest(allLineMap)
  fewestSubsets <- as(fewestMaps[["Fewest-Line Map (Subsets)"]], "sf")
  fewestMinimal <- as(fewestMaps[["Fewest-Line Map (Minimal)"]], "sf")

  # All line
  expect_length(colnames(allLineMapSf), 4L)
  expect_identical(nrow(allLineMapSf), 7L)

  # Fewest (Subsets)
  expect_length(colnames(fewestSubsets), 4L)
  expect_identical(nrow(fewestSubsets), 1L)

  # Fewest (Minimal)
  expect_length(colnames(fewestMinimal), 4L)
  expect_identical(nrow(fewestMinimal), 1L)
})
