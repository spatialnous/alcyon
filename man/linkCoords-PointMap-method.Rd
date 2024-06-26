% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PointMap.R
\docType{methods}
\name{linkCoords,PointMap-method}
\alias{linkCoords,PointMap-method}
\title{Link two PointMap Cells (coordinates)}
\usage{
\S4method{linkCoords}{PointMap}(map, fromX, fromY, toX, toY, copyMap = TRUE)
}
\arguments{
\item{map}{A PointMap}

\item{fromX}{X coordinate of the first link point}

\item{fromY}{Y coordinate of the first link point}

\item{toX}{X coordinate of the second link point}

\item{toY}{Y coordinate of the second link point}

\item{copyMap}{Optional. Copy the internal sala map}
}
\value{
A new PointMap with linked points
}
\description{
Link two cells on a PointMap using the point coordinates
}
\examples{
mifFile <- system.file(
    "extdata", "testdata", "gallery",
    "gallery_lines.mif",
    package = "alcyon"
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
  )
linkCoords(pointMap, 1.74, 6.7, 5.05, 5.24)
}
