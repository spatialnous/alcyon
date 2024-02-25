# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

depth <- function(map,
                  depthType,
                  fromX,
                  fromY,
                  verbose = FALSE) {
  if (!(depthType %in% Traversal)) {
    stop("Unknown depth type: ", depthType)
  }

  if ("PointMap" %in% class(map)) {

  } else if ("AxialShapeGraph" %in% class(map)) {
    return(Rcpp_axialStepDepth(map@ptr, depthType, fromX, fromY));
  } else if ("SegmentShapeGraph" %in% class(map)) {
    return(Rcpp_segmentStepDepth(map@ptr, depthType, fromX, fromY));
  } else {
    stop("Can only run depth on Axial or Segment ShapeGraphs and PointMaps")
  }
}
