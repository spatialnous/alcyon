% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepareVGA.R
\name{fillGrid}
\alias{fillGrid}
\title{Fill a LatticeMap's grid starting from one or more points}
\usage{
fillGrid(latticeMap, fillX, fillY, copyMap = TRUE, verbose = FALSE)
}
\arguments{
\item{latticeMap}{The input LatticeMap}

\item{fillX}{X coordinate of the fill points}

\item{fillY}{Y coordinate of the fill points}

\item{copyMap}{Optional. Copy the internal sala map}

\item{verbose}{Optional. Show more information of the process.}
}
\value{
A new LatticeMap with filled points
}
\description{
Fill a LatticeMap's grid starting from one or more points
}
\examples{
mifFile <- system.file(
    "extdata", "testdata", "simple",
    "simple_interior.mif",
    package = "alcyon"
  )
  sfMap <- st_read(mifFile,
    geometry_column = 1L, quiet = TRUE
  )
  shapeMap <- as(sfMap[, vector()], "ShapeMap")
lineStringMap <- as(sfMap, "sf")
mapRegion <- sf::st_bbox(lineStringMap)
latticeMap <- createGrid(
  minX = mapRegion[["xmin"]],
  minY = mapRegion[["ymin"]],
  maxX = mapRegion[["xmax"]],
  maxY = mapRegion[["ymax"]],
  gridSize = 0.04
)
latticeMap <- blockLines(
  latticeMap = latticeMap,
  lineStringMap = lineStringMap[vector()]
)
fillGrid(
  latticeMap = latticeMap,
  fillX = 3.01,
  fillY = 6.7
)
}
