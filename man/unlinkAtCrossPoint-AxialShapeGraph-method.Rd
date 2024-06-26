% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AxialShapeGraph.R
\docType{methods}
\name{unlinkAtCrossPoint,AxialShapeGraph-method}
\alias{unlinkAtCrossPoint,AxialShapeGraph-method}
\title{Unlink two Axial Lines (crosspoint)}
\usage{
\S4method{unlinkAtCrossPoint}{AxialShapeGraph}(map, x, y, copyMap = TRUE)
}
\arguments{
\item{map}{An Axial ShapeGraph}

\item{x}{X coordinate of the unlink crossing point}

\item{y}{Y coordinate of the unlink crossing point}

\item{copyMap}{Optional. Copy the internal sala map}
}
\value{
A new Axial ShapeGraph with unlinked lines
}
\description{
Unlink two crossing lines on an Axial ShapeGraph at the crossing point
}
\examples{
mifFile <- system.file(
    "extdata", "testdata", "barnsbury",
    "barnsbury_small_axial_original.mif",
    package = "alcyon"
  )
  sfMap <- st_read(mifFile,
    geometry_column = 1L, quiet = TRUE
  )
  shapeGraph <- as(sfMap, "AxialShapeGraph")
unlinkAtCrossPoint(shapeGraph, 530925.0, 184119.0)
}
