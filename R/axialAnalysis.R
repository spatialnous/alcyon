# Copyright 2019 Fani Kostourou
# Copyright 2019-2024 Petros Koutsolampros
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
                         keepGraph = FALSE,
                         verbose = FALSE) {
    mod = Rcpp::Module("aedon_module", "aedon")
    numRadii = sapply(radii, function(r) {
        if (r == "n") {
            return(-1)
        } else {
            return(as.numeric(r))
        }
    })

    weightByIdx = NULL
    if (weightByAttribute != "") {
        weightByIdx = which(names(lineStringMap) == weightByAttribute)[1]
    }
    shapeMap = aedon:::toShapeMap(lineStringMap, weightByIdx)
    shapeGraph = aedon:::toAxialShapeGraph(shapeMap)

    attrNamesBefore = mod$getAttributeNames(shapeGraph)

    expectdAttrName = NULL
    if (!is.null(weightByIdx)) {
        expectdAttrName = aedon:::getSFShapeMapExpectedColName(lineStringMap, weightByIdx)
    }

    aedon:::runAxialAnalysis(
        shapeGraph,
        numRadii,
        expectdAttrName,
        includeChoice,
        includeLocal,
        includeIntermediateMetrics
    )

    attrNamesAfter = mod$getAttributeNames(shapeGraph)
    namesDiff = attrNamesAfter[!(attrNamesAfter %in% attrNamesBefore)]
    df = as.data.frame(do.call(cbind,
                               mod$getAttributeData(shapeGraph, namesDiff)))
    row.names(df) =
        mod$getAttributeData(shapeGraph, "df_row_name")[["df_row_name"]]
    result = list(data = df[row.names(lineStringMap),],
                  graph = NA)
    return(result)

}
