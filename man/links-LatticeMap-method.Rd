% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LatticeMap.R
\docType{methods}
\name{links,LatticeMap-method}
\alias{links,LatticeMap-method}
\title{Get the LatticeMap links}
\usage{
\S4method{links}{LatticeMap}(map)
}
\arguments{
\item{map}{A LatticeMap}
}
\value{
A matrix with the linked refs
}
\description{
Get the LatticeMap links
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
linkRefs(latticeMap, 1835056L, 7208971L)
links(latticeMap)
}
