# SPDX-FileCopyrightText: 2019 Fani Kostourou
# SPDX-FileCopyrightText: 2019-2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

axialAnalysis <- function(lineStringMap,
                          radii,
                          weightByAttribute = "",
                          includeChoice = FALSE,
                          includeLocal = FALSE,
                          includeIntermediateMetrics = FALSE,
                          keepGraph = FALSE,
                          verbose = FALSE) {
  mod <- Rcpp::Module("alcyon_module", "alcyon")
  numRadii <- vapply(radii, function(r) {
    if (r == "n") {
      return(-1L)
    } else {
      return(as.integer(r))
    }
  }, FUN.VALUE = 1L)

  weightByIdx <- NULL
  if (weightByAttribute != "") {
    weightByIdx <- which(names(lineStringMap) == weightByAttribute)[[1L]]
  }
  shapeMap <- Rcpp_toShapeMap(lineStringMap, weightByIdx)
  shapeGraph <- Rcpp_toAxialShapeGraph(shapeMap)

  attrNamesBefore <- mod$getAttributeNames(shapeGraph)

  expectdAttrName <- NULL
  if (!is.null(weightByIdx)) {
    expectdAttrName <- Rcpp_getSFShapeMapExpectedColName(
      lineStringMap,
      weightByIdx
    )
  }

  Rcpp_runAxialAnalysis(
    shapeGraph,
    numRadii,
    expectdAttrName,
    includeChoice,
    includeLocal,
    includeIntermediateMetrics
  )

  attrNamesAfter <- mod$getAttributeNames(shapeGraph)
  namesDiff <- attrNamesAfter[!(attrNamesAfter %in% attrNamesBefore)]
  df <- as.data.frame(do.call(
    cbind,
    mod$getAttributeData(shapeGraph, namesDiff)
  ))
  row.names(df) <-
    mod$getAttributeData(shapeGraph, "df_row_name")[["df_row_name"]]
  result <- list(
    data = df[row.names(lineStringMap), ],
    graph = NA
  )
  return(result)
}
