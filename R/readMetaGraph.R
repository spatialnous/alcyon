# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

# Reads a metagraph into a bunch of ShapeMaps/ShapeGraphs

readMetaGraph <- function(fileName) {
  mod <- Rcpp::Module("alcyon_module", "alcyon")
  mgraphData <- mod$readMetaGraph(fileName)

  # convert the shapemap pointers to the ShapeMap class
  # provided by the package
  mgraphData$shapeMaps
}
