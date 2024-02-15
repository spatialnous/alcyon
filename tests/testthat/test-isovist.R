# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

context("Isovists tests")

test_that("Isovists in C++", {
  lineStringMap <- st_read(
    system.file(
      "extdata", "testdata", "gallery",
      "gallery_lines.mif",
      package = "alcyon"
    ),
    geometry_column = 1L, quiet = TRUE
  )

  shapeGraph <- sfToShapeMap(
    lineStringMap,
    keepAttributes = vector(mode = "integer")
  )

  isovistMap <- Rcpp_makeIsovists(
    shapeGraph,
    matrix(c(3.01, 6.7), nrow = 1L),
    0.0,
    3.14,
    FALSE
  )

  polygonCoords <- Rcpp_ShapeMap_getShapesAsPolygonCoords(isovistMap)

  sfGeom <- st_sfc(lapply(polygonCoords, function(polyCoords) {
    st_polygon(list(polyCoords), dim = "XY")
  }))

  attrNames <- Rcpp_ShapeMap_getAttributeNames(isovistMap)
  isovists <- st_sf(Rcpp_ShapeMap_getAttributeData(isovistMap, attrNames),
    geometry = sfGeom
  )

  expect_equal(st_area(isovists[1L, ]), 0.5186441, tolerance = 0.00001)

  centroid <- st_centroid(isovists[1L, "geometry"])[[1L]][[1L]]
  expect_equal(centroid[[1L]], 3.605456, tolerance = 0.00001)
  expect_equal(centroid[[2L]], 6.4919, tolerance = 0.00001)

  polygonPointMatrix <- isovists[1L, "geometry"][[1L]][[1L]][[1L]]
  expect_identical(
    dim(polygonPointMatrix),
    c(56L, 2L)
  )
})

test_that("Isovists in R", {
  lineStringMap <- st_read(
    system.file(
      "extdata", "testdata", "gallery",
      "gallery_lines.mif",
      package = "alcyon"
    ),
    geometry_column = 1L, quiet = TRUE
  )

  isovists <- isovist(
    lineStringMap,
    x = c(3.01, 1.3),
    y = c(6.70, 5.2),
    angle = 0.0,
    viewAngle = 3.14,
    FALSE
  )

  expect_equal(st_area(isovists[1L, ]), 0.5186, tolerance = 0.0001)

  centroid <- st_centroid(isovists[1L, "geometry"])[[1L]][[1L]]
  expect_equal(centroid[[1L]], 3.6054, tolerance = 0.0001)
  expect_equal(centroid[[2L]], 6.4919, tolerance = 0.0001)

  polygonPointMatrix <- isovists[1L, "geometry"][[1L]][[1L]][[1L]]
  expect_identical(
    dim(polygonPointMatrix),
    c(56L, 2L)
  )


  expect_equal(st_area(isovists[2L, ]), 0.1531, tolerance = 0.0001)

  centroid2 <- st_centroid(isovists[2L, "geometry"])[[1L]][[1L]]
  expect_equal(centroid2[[1L]], 1.7125, tolerance = 0.0001)
  expect_equal(centroid2[[2L]], 5.2319, tolerance = 0.0001)

  polygonPointMatrix <- isovists[2L, "geometry"][[1L]][[1L]][[1L]]
  expect_identical(
    dim(polygonPointMatrix),
    c(47L, 2L)
  )
})
