// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "salalib/pointdata.h"
#include "salalib/gridproperties.h"

#include "communicator.h"

#include <Rcpp.h>

RCPP_EXPOSED_CLASS(PointMap);

// [[Rcpp::export("Rcpp_PointMap_makeGraph")]]
bool makeGraph(Rcpp::XPtr<PointMap> pointMap,
               Rcpp::XPtr<ShapeMap> boundaryMap,
               bool boundaryGraph,
               double maxVisibility) {
  bool graphMade = false;
  try {
    std::vector<Line> lines;
    for (auto line: boundaryMap->getAllShapesAsLines()) {
      lines.emplace_back(line.start(), line.end());
    }
    graphMade = pointMap->sparkGraph2(getCommunicator(true).get(),
                                      lines,
                                      boundaryGraph,
                                      maxVisibility);
  } catch (Communicator::CancelledException) {
    graphMade = false;
  }
  return graphMade;
}

// [[Rcpp::export("Rcpp_PointMap_unmakeGraph")]]
void unmakeGraph(Rcpp::XPtr<PointMap> pointMap,
                 bool removeLinksWhenUnmaking) {
  if (!pointMap->isProcessed()) {
    Rcpp::stop("Current map has not had its graph",
               "made so there's nothing to unmake");
  }

  pointMap->unmake(removeLinksWhenUnmaking);
}

// [[Rcpp::export("Rcpp_PointMap_fill")]]
void fill(Rcpp::XPtr<PointMap> pointMap,
          Rcpp::XPtr<ShapeMap> boundaryMap,
          Rcpp::NumericMatrix pointCoords) {
  if (pointCoords.rows() == 0) {
    Rcpp::stop("No data provided in point coordinates matrix");
  }

  auto region = pointMap->getRegion();
  for (int r = 0; r < pointCoords.rows(); ++r) {
    auto coordRow = pointCoords.row(r);
    Point2f p(coordRow[0], coordRow[1]);
    if (!region.contains(p)) {
      Rcpp::stop("Point outside of target region: ", p.x, p.y);
    }
  }

  std::vector<Line> lines;
  for (auto line: boundaryMap->getAllShapesAsLines()) {
    lines.emplace_back(line.start(), line.end());
  }
  for (int r = 0; r < pointCoords.rows(); ++r) {
    auto coordRow = pointCoords.row(r);
    Point2f p(coordRow[0], coordRow[1]);
    pointMap->makePoints(lines, p, 0, getCommunicator(true).get());
  }
}


// [[Rcpp::export("Rcpp_PointMap_createFromGrid")]]
Rcpp::XPtr<PointMap> createFromGrid(Rcpp::XPtr<ShapeMap> boundsMap,
                                    double gridSize) {

  if (gridSize <= 0) {
    Rcpp::stop("gridSize can not be less or equal to zero (", gridSize,
               " given)");
  }
  Rcpp::XPtr<PointMap> pointMap = Rcpp::XPtr<PointMap>(new PointMap(
    boundsMap->getRegion(), "PointMap"
  ));
  // Create a new pointmap and set tha grid
  QtRegion r = boundsMap->getRegion();

  GridProperties gp(std::max(r.width(), r.height()));
  if (gridSize > gp.getMax() || gridSize < gp.getMin()) {
    Rcpp::stop("Chosen grid spacing ", gridSize,
               " is outside of the expected interval of ", gp.getMin(),
               " <= spacing <= ", gp.getMax());
  }

  pointMap->setGrid(gridSize, Point2f(0.0, 0.0));

  return pointMap;
}

