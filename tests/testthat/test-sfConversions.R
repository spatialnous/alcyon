# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

context("Segment Analysis tests")

test_that("sf linestrings to ShapeMap and back", {
  lineStringMap <- loadSmallAxialLinesAsSf()$sf

  numericCols <- which(unlist(
    lapply(lineStringMap, is.numeric),
    use.names = FALSE
  ))

  shapeMap <- as(lineStringMap, "ShapeMap")
  expectedColNames <- c(
    "Ref",
    "df_row_name",
    paste0("df_", numericCols, "_", names(lineStringMap)[numericCols])
  )
  expect_identical(
    Rcpp_ShapeMap_getAttributeNames(shapeMap@ptr),
    expectedColNames
  )

  newLineStringMap <- as(shapeMap, "sf")
  expect_named(newLineStringMap, c(expectedColNames, "geometry"))
})

test_that("sf linestrings to Axial Map", {
  lineStringMap <- loadSmallAxialLinesAsSf()$sf

  numericCols <- which(unlist(
    lapply(lineStringMap, is.numeric),
    use.names = FALSE
  ))
  # only pick around half the columns
  numericCols <- numericCols[seq_len(length(numericCols) / 2L)]

  shapeGraph <- as(lineStringMap[numericCols], "AxialShapeGraph")

  expectedColNames <- c(
    "Ref",
    "Connectivity",
    "Line Length",
    "Data Map Ref",
    "df_row_name",
    paste0("df_", numericCols, "_", names(lineStringMap)[numericCols])
  )
  attrNames <- Rcpp_ShapeMap_getAttributeNames(shapeGraph@ptr)
  expect_identical(expectedColNames, attrNames)

  newLineStringMap <- as(shapeGraph, "sf")
  expect_named(newLineStringMap, c(expectedColNames, "geometry"))
})

test_that("sf linestrings to Segment Map through Axial Map and back", {
  lineStringMap <- loadSmallAxialLinesAsSf()$sf

  numericCols <- which(unlist(
    lapply(lineStringMap, is.numeric),
    use.names = FALSE
  ))
  # only pick around half the columns
  numericCols <- numericCols[seq_len(length(numericCols) / 2L)]
  axialMap <- as(lineStringMap[numericCols], "AxialShapeGraph")
  segmentGraph <- axialToSegmentShapeGraph(
    axialMap,
    stubRemoval = 0.4
  )

  expectedColNames <- c(
    "Ref",
    "Axial Line Ref",
    "Segment Length",
    "Angular Connectivity",
    "Connectivity",
    "Axial Connectivity",
    "Axial Line Length",
    "Axial Data Map Ref",
    "Axial df_row_name",
    paste0("Axial df_", numericCols, "_", names(lineStringMap)[numericCols])
  )
  attrNames <- Rcpp_ShapeMap_getAttributeNames(segmentGraph@ptr)
  expect_identical(expectedColNames, attrNames)

  newLineStringMap <- as(segmentGraph, "sf")
  expect_named(newLineStringMap, c(expectedColNames, "geometry"))
})

test_that("sf linestrings to Segment Map and back", {
  lineStringMap <- loadSmallSegmentLinesAsSf()$sf

  numericCols <- which(unlist(
    lapply(lineStringMap, is.numeric),
    use.names = FALSE
  ))
  # only pick around half the columns
  numericCols <- numericCols[seq_len(length(numericCols) / 2L)]
  segmentGraph <- as(lineStringMap[numericCols], "SegmentShapeGraph")

  expectedColNames <- c(
    "Ref",
    "Axial Line Ref",
    "Segment Length",
    "Data Map Ref",
    paste0("df_", numericCols, "_", names(lineStringMap)[numericCols]),
    "df_row_name",
    "Angular Connectivity",
    "Connectivity"
  )
  attrNames <- Rcpp_ShapeMap_getAttributeNames(segmentGraph@ptr)
  expect_identical(expectedColNames, attrNames)

  newLineStringMap <- as(segmentGraph, "sf")
  expect_named(newLineStringMap, c(expectedColNames, "geometry"))
})

test_that("sf polygons to Shape Map and back", {
  polyMap <- loadInteriorPolygonsAsSf()$sf
  shapeMap <- as(polyMap[vector()], "ShapeMap")
  polygons <- shapeMapToPolygonSf(shapeMap)

  expect_equal(st_area(polygons[1L, ]), 0.285, tolerance = 0.0001)

  centroid <- st_centroid(polygons[1L, "geometry"])[[1L]][[1L]]
  expect_equal(centroid[[1L]], 3.0605, tolerance = 0.0001)
  expect_equal(centroid[[2L]], 6.6830, tolerance = 0.0001)

  polygonPointMatrix <- polygons[1L, "geometry"][[1L]][[1L]][[1L]]
  expect_identical(
    dim(polygonPointMatrix),
    c(5L, 2L)
  )

  expect_equal(st_area(polygons[2L, ]), 0.2227, tolerance = 0.0001)

  centroid <- st_centroid(polygons[2L, "geometry"])[[1L]][[1L]]
  expect_equal(centroid[[1L]], 1.1755, tolerance = 0.0001)
  expect_equal(centroid[[2L]], 5.2960, tolerance = 0.0001)

  polygonPointMatrix <- polygons[2L, "geometry"][[1L]][[1L]][[1L]]
  expect_identical(
    dim(polygonPointMatrix),
    c(5L, 2L)
  )
})
