% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepareVGA.R
\name{blockLines}
\alias{blockLines}
\title{Block lines on a LatticeMap}
\usage{
blockLines(latticeMap, lineStringMap, copyMap = TRUE, verbose = FALSE)
}
\arguments{
\item{latticeMap}{The input LatticeMap}

\item{lineStringMap}{Map of lines, either a ShapeMap, or an sf lineString map}

\item{copyMap}{Optional. Copy the internal sala map}

\item{verbose}{Optional. Show more information of the process.}
}
\value{
A new LatticeMap with points as they have been blocked by the lines
}
\description{
Takes a LatticeMap and a ShapeMap with lines and blocks the cells on the
LatticeMap where the lines pass.
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
blockLines(
  latticeMap = latticeMap,
  lineStringMap = lineStringMap[vector()]
)
}
