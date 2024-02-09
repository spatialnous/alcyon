# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

context("ShapeMap tests")

test_that("Create ShapeMap", {
  shapeMapName <- "Test ShapeMap"
  shp <- ShapeMap(shapeMapName)
  expect_identical(shapeMapName, name(shp))
})
