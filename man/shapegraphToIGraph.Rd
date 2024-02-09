% SPDX-FileCopyrightText: 2019 Fani Kostourou
% SPDX-FileCopyrightText: 2019 Petros Koutsolampros
%
% SPDX-License-Identifier: GPL-3.0-or-later

\name{shapegraphToIGraph}
\alias{shapegraphToIGraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Conversion of shapegraph to igraph
}
\description{
Creates igraph based on the connections and the x,y coordinates of the centroids of shapes in a shapegraph (axial,segment, convex). Specify weightcolumn to assign weight to graph edges.
}
\usage{
shapegraphToIGraph(graphFile, weightcolumn = NA)
}
\arguments{
  \item{graphFile}{
    The input graph file.
  }
  \item{weightcolumn}{
    Optional.The variable used to assign weight to graph edges
  }
}
\details{
If weightcolumn is provided, edge connections weight is calculated by taking the average of the variable of the connected nodes.
}
\value{
  Returns graph.data.frame.
}
\author{
Fani Kostourou
Petros Koutsolampros
}
