% SPDX-FileCopyrightText: 2019 Petros Koutsolampros
%
% SPDX-License-Identifier: GPL-3.0-or-later

\name{makeVGAGraph}
\alias{makeVGAGraph}
\alias{unmakeVGAGraph}
\title{
Make/Unmake the VGA Graph.
}
\description{
Makes or unmakes the graph.
}
\usage{
makeVGAGraph(graphFileIn, graphFileOut = NA, maxVisibility = NA, boundaryGraph = FALSE,
             cliPath = getDefaultCLILocation(), verbose = FALSE)
unmakeVGAGraph(graphFileIn, graphFileOut = NA, removeLinks = FALSE,
               cliPath = getDefaultCLILocation(), verbose = FALSE)
}
\arguments{
  \item{graphFileIn}{
    The input graph file.
  }
  \item{graphFileOut}{
    The output graph file. If ommited will use the input file.
  }
  \item{maxVisibility}{
    Set a maximum visibility distance when making the graph.
  }
  \item{boundaryGraph}{
    Create a boundary graph when making the graph.
  }
  \item{removeLinks}{
    Remove existing links when unmaking the graph.
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
