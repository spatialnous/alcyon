# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

rxgn_loadSmallSegmentLines <- function() {
  ex <- "mifFile = system.file(
  \"extdata\", \"testdata\", \"barnsbury\",
  \"barnsbury_small_segment.mif\",
  package = \"alcyon\"
)
sfMap <- st_read(mifFile,
  geometry_column = 1L, quiet = TRUE
)
shapeGraph <- as(sfMap, \"SegmentShapeGraph\")"
return(strsplit(ex, split = "\n")[[1]]) # needed to have line jumps in the doc
}

rxgn_loadSmallAxialLines <- function() {
  ex <- "mifFile = system.file(
  \"extdata\", \"testdata\", \"barnsbury\",
  \"barnsbury_small_axial.mif\",
  package = \"alcyon\"
)
sfMap <- st_read(mifFile,
  geometry_column = 1L, quiet = TRUE
)
shapeGraph <- as(sfMap, \"AxialShapeGraph\")"
return(strsplit(ex, split = "\n")[[1]]) # needed to have line jumps in the doc
}

rxgn_loadInteriorLinesAsShapeMap <- function() {
  ex <- "mifFile = system.file(
  \"extdata\", \"testdata\", \"gallery\",
  \"gallery_lines.mif\",
  package = \"alcyon\"
)
sfMap <- st_read(mifFile,
  geometry_column = 1L, quiet = TRUE
)
shapeMap <- as(sfMap[, c()], \"ShapeMap\")"
return(strsplit(ex, split = "\n")[[1]]) # needed to have line jumps in the doc
}

rxgn_loadInteriorLinesAsPointMap <- function() {
  ex <- "mifFile = system.file(
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
return(strsplit(ex, split = "\n")[[1]]) # needed to have line jumps in the doc
}
