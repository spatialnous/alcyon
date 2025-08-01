% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LatticeMap.R
\docType{methods}
\name{connections,LatticeMap-method}
\alias{connections,LatticeMap-method}
\title{Get the LatticeMap connections}
\usage{
\S4method{connections}{LatticeMap}(map)
}
\arguments{
\item{map}{A LatticeMap}
}
\value{
A matrix with the connected refs
}
\description{
Get the LatticeMap connections
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
  latticeMap <- makeVGALatticeMap(
    sfMap,
    gridSize = 0.04,
    fillX = 3.01,
    fillY = 6.7,
    maxVisibility = NA,
    boundaryGraph = FALSE,
    verbose = FALSE
  )
# plot the first 100 connections only
head(connections(latticeMap), 100)
}
