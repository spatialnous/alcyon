# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

context("Conversion tests")

test_that("sf line map to ShapeMap", {
  mod <- Rcpp::Module("alcyon_module", "alcyon")
  lineStringMap <- st_read(
    system.file(
      "extdata", "testdata", "barnsbury", "barnsbury_small_axial.mif",
      package = "alcyon"
    ),
    geometry_column = 1L, quiet = TRUE
  )

  shapeMap <- Rcpp_toShapeMap(lineStringMap, c(1L, 2L))

  expectedColNames <- c(
    "Ref",
    "df_row_name",
    "df_1_Depthmap_Ref",
    "df_2_Choice"
  )
  attrNames <- mod$getAttributeNames(shapeMap)
  expect_identical(expectedColNames, attrNames)

  firstCol <- attrNames[[1L]]
  firstColData <- mod$getAttributeData(shapeMap, firstCol)[[firstCol]]
  expect_length(firstColData, nrow(lineStringMap))
})

test_that("sf line map to Axial ShapeGraph", {
  mod <- Rcpp::Module("alcyon_module", "alcyon")
  lineStringMap <- st_read(
    system.file(
      "extdata", "testdata", "barnsbury", "barnsbury_small_axial.mif",
      package = "alcyon"
    ),
    geometry_column = 1L, quiet = TRUE
  )

  shapeMap <- Rcpp_toShapeMap(lineStringMap, c(1L, 2L))
  shapeGraph <- Rcpp_toAxialShapeGraph(shapeMap)

  expectedColNames <- c(
    "Ref",
    "Connectivity",
    "Line Length",
    "Data Map Ref",
    "df_1_Depthmap_Ref",
    "df_2_Choice",
    "df_row_name"
  )
  attrNames <- mod$getAttributeNames(shapeGraph)
  expect_identical(expectedColNames, attrNames)

  firstCol <- attrNames[[1L]]
  firstColData <- mod$getAttributeData(shapeGraph, firstCol)[[firstCol]]
  expect_length(firstColData, nrow(lineStringMap))

  axialConnections <- mod$getAxialConnections(shapeGraph)
  expect_length(axialConnections$from, 1498L)
})

test_that("sf line map to Segment ShapeGraph", {
  mod <- Rcpp::Module("alcyon_module", "alcyon")
  lineStringMap <- st_read(
    system.file(
      "extdata", "testdata", "barnsbury", "barnsbury_small_axial.mif",
      package = "alcyon"
    ),
    geometry_column = 1L, quiet = TRUE
  )

  shapeMap <- Rcpp_toShapeMap(lineStringMap, c(1L, 2L))
  shapeGraph <- Rcpp_toAxialShapeGraph(shapeMap)

  segmentMap <- Rcpp_axialToSegment(shapeGraph)

  expectedColNames <- c(
    "Ref",
    "Axial Line Ref",
    "Segment Length",
    "Angular Connectivity",
    "Connectivity",
    "Axial Connectivity",
    "Axial Line Length",
    "Axial Data Map Ref",
    "Axial df_1_Depthmap_Ref",
    "Axial df_2_Choice",
    "Axial df_row_name"
  )
  attrNames <- mod$getAttributeNames(segmentMap)
  expect_identical(expectedColNames, attrNames)

  firstCol <- attrNames[[1L]]
  firstColData <- mod$getAttributeData(segmentMap, firstCol)[[firstCol]]
  expect_length(firstColData, 1559L)

  segmentConnections <- mod$getSegmentConnections(segmentMap)
  expect_length(segmentConnections$from, 8988L)
})
