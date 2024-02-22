# SPDX-FileCopyrightText: 2019 Fani Kostourou
# SPDX-FileCopyrightText: 2019-2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

axialAnalysis <- function(shapeGraph,
                          radii,
                          weightByAttribute = "",
                          includeChoice = FALSE,
                          includeLocal = FALSE,
                          includeIntermediateMetrics = FALSE,
                          keepGraph = FALSE,
                          verbose = FALSE) {
  numRadii <- vapply(radii, function(r) {
    if (r == "n") {
      return(-1L)
    } else {
      return(as.integer(r))
    }
  }, FUN.VALUE = 1L)

  return(Rcpp_runAxialAnalysis(
    shapeGraph@ptr,
    numRadii,
    weightByAttribute,
    includeChoice,
    includeLocal,
    includeIntermediateMetrics
  ))
}

axialAnalysisSf <- function(lineStringMap,
                            radii,
                            weightByAttribute = "",
                            includeChoice = FALSE,
                            includeLocal = FALSE,
                            includeIntermediateMetrics = FALSE,
                            keepGraph = FALSE,
                            verbose = FALSE) {
  weightByIdx <- NULL
  if (weightByAttribute != "") {
    weightByIdx <- which(names(lineStringMap) == weightByAttribute)[[1L]]
  }
  shapeGraph <- sfToAxialShapeGraph(lineStringMap,
    keepAttributes = weightByIdx
  )

  attrNamesBefore <- Rcpp_ShapeMap_getAttributeNames(shapeGraph@ptr)

  expectdAttrName <- NULL
  if (!is.null(weightByIdx)) {
    expectdAttrName <- Rcpp_getSfShapeMapExpectedColName(
      lineStringMap,
      weightByIdx
    )
  }

  axialAnalysis(
    shapeGraph,
    radii,
    expectdAttrName,
    includeChoice,
    includeLocal,
    includeIntermediateMetrics,
    keepGraph,
    verbose
  )

  attrNamesAfter <- Rcpp_ShapeMap_getAttributeNames(shapeGraph@ptr)
  namesDiff <- attrNamesAfter[!(attrNamesAfter %in% attrNamesBefore)]
  df <- as.data.frame(do.call(
    cbind,
    Rcpp_ShapeMap_getAttributeData(shapeGraph@ptr, namesDiff)
  ))
  row.names(df) <-
    Rcpp_ShapeMap_getAttributeData(
      shapeGraph@ptr,
      "df_row_name"
    )[["df_row_name"]]
  result <- list(
    data = df[row.names(lineStringMap), ],
    graph = NA
  )
  return(result)
}
