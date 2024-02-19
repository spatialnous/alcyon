// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "salalib/pointdata.h"
#include "salalib/gridproperties.h"

#include "communicator.h"

#include <Rcpp.h>

RCPP_EXPOSED_CLASS(PointMap);

// [[Rcpp::export("Rcpp_PointMap_createFromGrid")]]
Rcpp::XPtr<PointMap> createFromGrid(double minX,
                                    double minY,
                                    double maxX,
                                    double maxY,
                                    double gridSize) {

  if (gridSize <= 0) {
    Rcpp::stop("gridSize can not be less or equal to zero (", gridSize,
               " given)");
  }
  // Create a new pointmap and set tha grid
  QtRegion r(Point2f(minX, minY) /* bottom left */,
             Point2f(maxX, maxY) /* top right */);

  Rcpp::XPtr<PointMap> pointMap = Rcpp::XPtr<PointMap>(new PointMap(
    r, "PointMap"
  ));

  GridProperties gp(std::max(r.width(), r.height()));
  if (gridSize > gp.getMax() || gridSize < gp.getMin()) {
    Rcpp::stop("Chosen grid spacing ", gridSize,
               " is outside of the expected interval of ", gp.getMin(),
               " <= spacing <= ", gp.getMax());
  }

  pointMap->setGrid(gridSize, Point2f(0.0, 0.0));

  return pointMap;
}

// [[Rcpp::export("Rcpp_PointMap_blockLines")]]
void blockLines(Rcpp::XPtr<PointMap> pointMap,
                Rcpp::XPtr<ShapeMap> boundaryMap) {

  std::vector<Line> lines;
  for (auto line: boundaryMap->getAllShapesAsLines()) {
    lines.emplace_back(line.start(), line.end());
  }
  pointMap->blockLines(lines);
}

// [[Rcpp::export("Rcpp_PointMap_fill")]]
void fill(Rcpp::XPtr<PointMap> pointMap,
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

  for (int r = 0; r < pointCoords.rows(); ++r) {
    auto coordRow = pointCoords.row(r);
    Point2f p(coordRow[0], coordRow[1]);
    pointMap->makePoints(p, 0, getCommunicator(true).get());
  }
}

// [[Rcpp::export("Rcpp_PointMap_makeGraph")]]
bool makeGraph(Rcpp::XPtr<PointMap> pointMap,
               bool boundaryGraph,
               double maxVisibility) {
  bool graphMade = false;
  try {
    graphMade = pointMap->sparkGraph2(getCommunicator(true).get(),
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

// [[Rcpp::export("Rcpp_PointMap_getName")]]
std::string pointMapGetName(Rcpp::XPtr<PointMap> pointMap) {
  return pointMap->getName();
}

// [[Rcpp::export("Rcpp_PointMap_getFilledPoints")]]
Rcpp::NumericMatrix getFilledPoints(Rcpp::XPtr<PointMap> pointMap) {
  const auto &attrTable = pointMap->getAttributeTable();
  int numCols = attrTable.getNumColumns();
  Rcpp::NumericMatrix coordsData(pointMap->getFilledPointCount(), 4 + numCols);
  Rcpp::CharacterVector colNames(3 + numCols);
  colNames[0] = "x";
  colNames[1] = "y";
  colNames[2] = "filled";
  colNames[3] = "Ref";
  for(int i = 0; i < numCols; ++i) {
    colNames[4 + i] = attrTable.getColumnName(i);
  }

  Rcpp::colnames(coordsData) = colNames;
  const auto &points = pointMap->getPoints();

  int rowIdx = 0;
  auto attrRowIt = attrTable.begin();
  for (auto &point: points) {
    if (!point.filled()) continue;
    const Rcpp::NumericMatrix::Row &row = coordsData( rowIdx , Rcpp::_ );
    row[0] = point.getLocation().x;
    row[1] = point.getLocation().y;
    row[2] = point.filled();
    row[3] = attrRowIt->getKey().value;
    for(int i = 0; i < numCols; ++i) {
      row[4 + i] = attrRowIt->getRow().getValue(i);
    }
    rowIdx++;
    attrRowIt++;
  }
  return coordsData;
}
