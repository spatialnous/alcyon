% SPDX-FileCopyrightText: 2019 Fani Kostourou
% SPDX-FileCopyrightText: 2019 Petros Koutsolampros
%
% SPDX-License-Identifier: GPL-3.0-or-later

\name{axialAnalysis}
\alias{axialAnalysis}
\title{
Axial Analysis
}
\description{
Calls the depthmapXcli to carry out Axial Analysis on the currently visible map
}
\usage{
axialAnalysis(lineStringMap,
              radii,
              weightByAttribute = "",
              includeChoice = FALSE,
              includeLocal = FALSE,
              includeIntermediateMetrics = FALSE,
              keepGraph = FALSE,
              verbose = FALSE)
}
\arguments{
  \item{lineStringMap}{
    The input line string sf map
  }
  \item{radii}{
    One or more radii to limit the analysis at.
  }
  \item{weightByAttribute}{
    Weigh the analysis using this particualr attribute
  }
  \item{includeChoice}{
    Set to TRUE to include choice.
  }
  \item{includeLocal}{
    Set to TRUE to include local measures.
  }
  \item{includeIntermediateMetrics}{
    Set to TRUE to include intermediate measures (RA, RRA and total depth).
  }
  \item{keepGraph}{
    Optional. Keep and return the graph.
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
Fani Kostourou
}
