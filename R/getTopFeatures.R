# SPDX-FileCopyrightText: 2019 Kimon Krenz
# SPDX-FileCopyrightText: 2019-2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

#' Extract top x percent of features
#'
#' Sorts features by a specific column and extracts the top x percent
#'
#' @param lineStringMap An sf lineString map
#' @param column The column to use to extract the features from
#' @param percent Percentage of features (to total) to extract
#' @return The lineString map filtered and sorted
#' @importFrom utils tail
#' @export
getTopFeatures <- function(lineStringMap,
                           column,
                           percent) {
  numberOfFeatures <- nrow(lineStringMap)
  orderedFeatureIDs <- order(lineStringMap[[column]])
  lineStringMap[tail(orderedFeatureIDs, percent * numberOfFeatures), ]
}
