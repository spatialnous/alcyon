# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

context("Segment Analysis tests")

test_that("sf linestrings to ShapeMap and back", {
  lineStringMap <- st_read(
    system.file(
      "extdata", "testdata", "barnsbury",
      "barnsbury_small_axial.mif",
      package = "alcyon"
    ),
    geometry_column = 1L, quiet = TRUE
  )

  numericCols <- which(unlist(
    lapply(lineStringMap, is.numeric),
    use.names = FALSE
  ))

  # only pick around half the columns
  numericCols <- numericCols[seq_len(length(numericCols) / 2L)]

  shapeMap <- sfToShapeMap(lineStringMap, numericCols)
  expectedColNames <- c(
    "Ref",
    "df_row_name",
    paste0("df_", numericCols, "_", names(lineStringMap)[numericCols])
  )
  expect_identical(
    Rcpp_ShapeMap_getAttributeNames(shapeMap@ptr),
    expectedColNames
  )

  newLineStringMap <- shapeMapTolineStringSf(shapeMap)
  expect_named(newLineStringMap, c(expectedColNames, "geometry"))
})

test_that("sf linestrings to Axial Map", {
  lineStringMap <- st_read(
    system.file(
      "extdata", "testdata", "barnsbury",
      "barnsbury_small_axial.mif",
      package = "alcyon"
    ),
    geometry_column = 1L, quiet = TRUE
  )

  numericCols <- which(unlist(
    lapply(lineStringMap, is.numeric),
    use.names = FALSE
  ))
  # only pick around half the columns
  numericCols <- numericCols[seq_len(length(numericCols) / 2L)]

  shapeGraph <- sfToAxialShapeGraph(lineStringMap, numericCols)

  # TODO: sala messes up the column order when copying data
  numericCols <- c(
    numericCols[numericCols > 9L],
    numericCols[numericCols <= 9L]
  )

  expectedColNames <- c(
    "Ref",
    "Connectivity",
    "Line Length",
    "Data Map Ref",
    paste0("df_", numericCols, "_", names(lineStringMap)[numericCols]),
    "df_row_name"
  )
  attrNames <- Rcpp_ShapeMap_getAttributeNames(shapeGraph@ptr)
  expect_identical(expectedColNames, attrNames)

  newLineStringMap <- axialShapeGraphToSf(shapeGraph)
  expect_named(newLineStringMap$map, c(expectedColNames, "geometry"))
})

test_that("sf linestrings to Segment Map through Axial Map and back", {
  lineStringMap <- st_read(
    system.file(
      "extdata", "testdata", "barnsbury",
      "barnsbury_small_axial.mif",
      package = "alcyon"
    ),
    geometry_column = 1L, quiet = TRUE
  )

  numericCols <- which(unlist(
    lapply(lineStringMap, is.numeric),
    use.names = FALSE
  ))
  # only pick around half the columns
  numericCols <- numericCols[seq_len(length(numericCols) / 2L)]
  segmentGraph <- sfToSegmentShapeGraph(
    lineStringMap,
    numericCols,
    throughAxial = TRUE
  )


  # TODO: sala messes up the column order when copying data
  numericCols <- c(
    numericCols[numericCols > 9L],
    numericCols[numericCols <= 9L]
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
    paste0("Axial df_", numericCols, "_", names(lineStringMap)[numericCols]),
    "Axial df_row_name"
  )
  attrNames <- Rcpp_ShapeMap_getAttributeNames(segmentGraph@ptr)
  expect_identical(expectedColNames, attrNames)

  newLineStringMap <- segmentShapeGraphToSf(segmentGraph)
  expect_named(newLineStringMap$map, c(expectedColNames, "geometry"))
})

test_that("sf linestrings to Segment Map and back", {
  lineStringMap <- st_read(
    system.file(
      "extdata", "testdata", "barnsbury",
      "barnsbury_small_axial.mif",
      package = "alcyon"
    ),
    geometry_column = 1L, quiet = TRUE
  )

  numericCols <- which(unlist(
    lapply(lineStringMap, is.numeric),
    use.names = FALSE
  ))
  # only pick around half the columns
  numericCols <- numericCols[seq_len(length(numericCols) / 2L)]
  segmentGraph <- sfToSegmentShapeGraph(lineStringMap, numericCols)

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

  newLineStringMap <- segmentShapeGraphToSf(segmentGraph)
  expect_named(newLineStringMap$map, c(expectedColNames, "geometry"))
})

test_that("sf polygons to Shape Map and back", {
  polyMap <- st_read(
    system.file(
      "extdata", "testdata", "gallery",
      "gallery_polys.mif",
      package = "alcyon"
    ),
    geometry_column = 1L, quiet = TRUE
  )
  shapeMap <- sfToShapeMap(polyMap)
  shapeMapToPolygongSf(shapeMap)
})
