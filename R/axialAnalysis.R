# Copyright 2019 Fani Kostourou
# Copyright 2019 Petros Koutsolampros
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

axialAnalysis = function(lineStringMap,
                         radii,
                         weightByAttribute = "",
                         includeChoice = FALSE,
                         includeLocal = FALSE,
                         includeIntermediateMetrics = FALSE,
                         verbose = FALSE) {
    mod = Rcpp::Module("aedon_module", "aedon")
    shapeMap = mod$toShapeMap(lineStringMap)
    shapeGraph = mod$toAxialShapeGraph(shapeMap)

    attrNamesBefore = mod$getAttributeNames(shapeGraph)

    aedon::runAxialAnalysis(shapeGraph, radii, weightByAttribute)

    attrNamesAfter = mod$getAttributeNames(shapeGraph)

}
