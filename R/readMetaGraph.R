# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

# Reads a metagraph into a bunch of ShapeMaps/ShapeGraphs

readMetaGraph <- function(fileName) {
  Rcpp_MetaGraph_read(fileName)
}
