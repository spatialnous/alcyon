# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

rxLoadSmallSegmentLines <- function() {
    ex <- "mifFile <- system.file(
    \"extdata\", \"testdata\", \"barnsbury\",
    \"barnsbury_small_segment_original.mif\",
    package = \"alcyon\"
  )
  sfMap <- st_read(mifFile,
    geometry_column = 1L, quiet = TRUE
  )
  shapeGraph <- as(sfMap, \"SegmentShapeGraph\")"
    return(strsplit(ex, split = "\n", fixed = TRUE)[[1L]])
}

rxLoadSmallAxialLines <- function() {
    ex <- "mifFile <- system.file(
    \"extdata\", \"testdata\", \"barnsbury\",
    \"barnsbury_small_axial_original.mif\",
    package = \"alcyon\"
  )
  sfMap <- st_read(mifFile,
    geometry_column = 1L, quiet = TRUE
  )
  shapeGraph <- as(sfMap, \"AxialShapeGraph\")"
    return(strsplit(ex, split = "\n", fixed = TRUE)[[1L]])
}

rxLoadInteriorLinesAsShapeMap <- function() {
    ex <- "mifFile <- system.file(
    \"extdata\", \"testdata\", \"gallery\",
    \"gallery_lines.mif\",
    package = \"alcyon\"
  )
  sfMap <- st_read(mifFile,
    geometry_column = 1L, quiet = TRUE
  )
  shapeMap <- as(sfMap[, vector()], \"ShapeMap\")"
    return(strsplit(ex, split = "\n", fixed = TRUE)[[1L]])
}

rxLoadInteriorLinesAsPointMap <- function() {
    ex <- "mifFile <- system.file(
    \"extdata\", \"testdata\", \"gallery\",
    \"gallery_lines.mif\",
    package = \"alcyon\"
  )
  sfMap <- st_read(mifFile,
    geometry_column = 1L, quiet = TRUE
  )
  pointMap <- makeVGAPointMap(
    sfMap,
    gridSize = 0.04,
    fillX = 3.01,
    fillY = 6.7,
    maxVisibility = NA,
    boundaryGraph = FALSE,
    verbose = FALSE
  )"
    return(strsplit(ex, split = "\n", fixed = TRUE)[[1L]])
}

rxLoadSimpleLinesAsShapeMap <- function() {
    ex <- "mifFile <- system.file(
    \"extdata\", \"testdata\", \"simple\",
    \"simple_interior.mif\",
    package = \"alcyon\"
  )
  sfMap <- st_read(mifFile,
    geometry_column = 1L, quiet = TRUE
  )
  shapeMap <- as(sfMap[, vector()], \"ShapeMap\")"
    return(strsplit(ex, split = "\n", fixed = TRUE)[[1L]])
}

rxLoadSimpleLinesAsPointMap <- function(gridSize = 0.04) {
    ex <- "mifFile <- system.file(
    \"extdata\", \"testdata\", \"simple\",
    \"simple_interior.mif\",
    package = \"alcyon\"
  )
  sfMap <- st_read(mifFile,
    geometry_column = 1L, quiet = TRUE
  )
  pointMap <- makeVGAPointMap(
    sfMap,
    gridSize = 0.5,
    fillX = 3.0,
    fillY = 6.0,
    maxVisibility = NA,
    boundaryGraph = FALSE,
    verbose = FALSE
  )"
    return(strsplit(ex, split = "\n", fixed = TRUE)[[1L]])
}
