% SPDX-FileCopyrightText: 2019 Petros Koutsolampros
%
% SPDX-License-Identifier: GPL-3.0-or-later

\name{createGrid}
\alias{createGrid}
\title{
Create a VGA grid
}
\description{
Creates a VGA-ready grid with a given cell size.
}
\usage{
createGrid(graphFileIn, graphFileOut, gridSize,
           cliPath = getDefaultCLILocation(), verbose = FALSE)
}
\arguments{
  \item{graphFileIn}{
    The input graph file.
  }
  \item{graphFileOut}{
    The output graph file. If ommited will use the input file.
  }
  \item{gridSize}{
    Cell size of the grid.
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
