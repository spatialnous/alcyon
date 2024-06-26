% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PointMap.R
\docType{methods}
\name{name,PointMap-method}
\alias{name,PointMap-method}
\title{Get the PointMap name}
\usage{
\S4method{name}{PointMap}(map)
}
\arguments{
\item{map}{A PointMap}
}
\value{
The name of the PointMap as a string
}
\description{
Get the PointMap name
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
name(pointMap)
}
