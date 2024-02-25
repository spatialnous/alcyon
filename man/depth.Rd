% SPDX-FileCopyrightText: 2019-2024 Petros Koutsolampros
%
% SPDX-License-Identifier: GPL-3.0-or-later

\name{depth}
\alias{depth}
\title{
  Step depth
}
\description{
Calculates step depth from a specific origin
}
\usage{
depth(map, depthType, fromX, fromY, verbose = FALSE)
}
\arguments{
  \item{map}{
    The map to traverse. Can be an Axial/Segment ShapeGraph or a PointMap.
  }
  \item{depthType}{
    The type of depth to measure steps in. Can be Traversal$metric, Traversal$angular, Traversal$topological or Traversal$tulip.
  }
  \item{fromX}{
    The x-value of the origin point.
  }
  \item{fromY}{
    The x-value of the origin point.
  }
  \item{verbose}{
    Optional. Show more information of the process.
  }
}
\value{
No value returned, process is between files.
}
\author{
Petros Koutsolampros
}
