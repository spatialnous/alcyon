% Copyright 2019 Fani Kostourou
% Copyright 2019 Petros Koutsolampros
%
% This program is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program. If not, see <https://www.gnu.org/licenses/>.

\name{axialAnalysis}
\alias{axialAnalysis}
\title{
Axial Analysis
}
\description{
Calls the depthmapXcli to carry out Axial Analysis on the currently visible map
}
\usage{
axialAnalysis(lineStringMap, radii, includeChoice = FALSE,
              includeLocal = FALSE, includeIntermediateMetrics = FALSE,
              cliPath = getDefaultCLILocation(), verbose = FALSE)
}
\arguments{
  \item{lineStringMap}{
    The input line string sf map
  }
  \item{radii}{
    One or more radii to limit the analysis at.
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
Fani Kostourou
}
