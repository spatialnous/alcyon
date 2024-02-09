% SPDX-FileCopyrightText: 2019 Petros Koutsolampros
%
% SPDX-License-Identifier: GPL-3.0-or-later

\name{fillGrid}
\alias{fillGrid}
\title{
Fill a VGA grid
}
\description{
Fills a VGA-ready grid at specfied points.
}
\usage{
fillGrid(graphFileIn, graphFileOut, fillX, fillY,
         cliPath = getDefaultCLILocation(), verbose = FALSE)
}
\arguments{
  \item{graphFileIn}{
    The input graph file.
  }
  \item{graphFileOut}{
    The output graph file. If ommited will use the input file.
  }
  \item{fillX}{
    Fill point(s) x-coordinate(s).
  }
  \item{fillY}{
    Fill point(s) end y-coordinate(s).
  }
  \item{cliPath}{
    Optional. Provide the path to a different CLI.
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
