# Copyright 2024 Petros Koutsolampros
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <https://www.gnu.org/licenses/>.

# Reads a metagraph into a bunch of ShapeMaps/ShapeGraphs

readMetaGraph <- function(fileName) {
  mod <- Rcpp::Module("alcyon_module", "alcyon")
  mgraphData <- mod$readMetaGraph(fileName)

  # convert the shapemap pointers to the ShapeMap class
  # provided by the package
  mgraphData$shapeMaps
}
