% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allToAllTraverse.R
\name{vgaThroughVision}
\alias{vgaThroughVision}
\title{Visibility Graph Analysis - Through Vision}
\usage{
vgaThroughVision(latticeMap, copyMap = TRUE)
}
\arguments{
\item{latticeMap}{A LatticeMap}

\item{copyMap}{Optional. Copy the internal sala map}
}
\value{
A new LatticeMap with the results included
}
\description{
Runs Visibility Graph Analysis to get the Through Vision metric
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
  latticeMap <- makeVGALatticeMap(
    sfMap,
    gridSize = 0.5,
    fillX = 3.0,
    fillY = 6.0,
    maxVisibility = NA,
    boundaryGraph = FALSE,
    verbose = FALSE
  )
vgaThroughVision(latticeMap)
}
