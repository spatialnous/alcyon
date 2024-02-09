# SPDX-FileCopyrightText: 2019 Kimon Krenz
# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

getTopFeatures <- function(spatialDataFrame,
                           column,
                           percent) {
  numberOfFeatures <- nrow(spatialDataFrame)
  orderedFeatureIDs <- order(spatialDataFrame[[column]])
  spatialDataFrame[tail(orderedFeatureIDs, percent * numberOfFeatures), ]
}
