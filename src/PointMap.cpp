// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

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
    Rcpp::stop("gridSize can not be less or equal to zero (%d given)",
               gridSize);
  }
  // Create a new pointmap and set tha grid
  QtRegion r(Point2f(minX, minY) /* bottom left */,
             Point2f(maxX, maxY) /* top right */);

  Rcpp::XPtr<PointMap> pointMap = Rcpp::XPtr<PointMap>(new PointMap(
    r, "PointMap"
  ));

  GridProperties gp(std::max(r.width(), r.height()));
  if (gridSize > gp.getMax() || gridSize < gp.getMin()) {
    Rcpp::stop("Chosen grid spacing %d is outside of the expected interval of"
                 "%d <= spacing <= %d", gridSize, gp.getMin(), gp.getMax());
  }

  pointMap->setGrid(gridSize, Point2f(0.0, 0.0));

  return pointMap;
}

// [[Rcpp::export("Rcpp_PointMap_blockLines")]]
void blockLines(Rcpp::XPtr<PointMap> pointMapPtr,
                Rcpp::XPtr<ShapeMap> boundaryMapPtr) {

  std::vector<Line> lines;
  for (auto line: boundaryMapPtr->getAllShapesAsLines()) {
    lines.emplace_back(line.start(), line.end());
  }
  pointMapPtr->blockLines(lines);
}

// [[Rcpp::export("Rcpp_PointMap_fill")]]
void fill(Rcpp::XPtr<PointMap> pointMapPtr,
          Rcpp::NumericMatrix pointCoords) {
  if (pointCoords.rows() == 0) {
    Rcpp::stop("No data provided in point coordinates matrix");
  }

  auto region = pointMapPtr->getRegion();
  for (int r = 0; r < pointCoords.rows(); ++r) {
    auto coordRow = pointCoords.row(r);
    Point2f p(coordRow[0], coordRow[1]);
    if (!region.contains(p)) {
      Rcpp::stop("Point (%d %d) outside of target pointmap region.", p.x, p.y);
    }
  }

  for (int r = 0; r < pointCoords.rows(); ++r) {
    auto coordRow = pointCoords.row(r);
    Point2f p(coordRow[0], coordRow[1]);
    pointMapPtr->makePoints(p, 0, getCommunicator(true).get());
  }
}

// [[Rcpp::export("Rcpp_PointMap_makeGraph")]]
bool makeGraph(Rcpp::XPtr<PointMap> pointMapPtr,
               bool boundaryGraph,
               double maxVisibility) {
  bool graphMade = false;
  try {
    graphMade = pointMapPtr->sparkGraph2(getCommunicator(true).get(),
                                         boundaryGraph,
                                         maxVisibility);
  } catch (Communicator::CancelledException) {
    graphMade = false;
  }
  return graphMade;
}

// [[Rcpp::export("Rcpp_PointMap_unmakeGraph")]]
void unmakeGraph(Rcpp::XPtr<PointMap> pointMapPtr,
                 bool removeLinksWhenUnmaking) {
  if (!pointMapPtr->isProcessed()) {
    Rcpp::stop("Current map has not had its graph "
               "made so there's nothing to unmake");
  }

  pointMapPtr->unmake(removeLinksWhenUnmaking);
}

// [[Rcpp::export("Rcpp_PointMap_getName")]]
std::string pointMapGetName(Rcpp::XPtr<PointMap> pointMapPtr) {
  return pointMapPtr->getName();
}

// [[Rcpp::export("Rcpp_PointMap_getLinks")]]
Rcpp::IntegerMatrix pointMapGetLinks(Rcpp::XPtr<PointMap> pointMapPtr) {
  auto mergedPixelPairs = pointMapPtr->getMergedPixelPairs();
  Rcpp::IntegerMatrix linkData(
      mergedPixelPairs.size(), 2L
  );
  Rcpp::colnames(linkData) = Rcpp::CharacterVector({
    "from", "to"
  });
  int rowIdx = 0;
  for (auto link: mergedPixelPairs) {
    const Rcpp::IntegerMatrix::Row &row = linkData( rowIdx , Rcpp::_ );
    row[0] = link.first;
    row[1] = link.second;
    rowIdx++;
  }
  return linkData;
}

// [[Rcpp::export("Rcpp_PointMap_getConnections")]]
Rcpp::IntegerMatrix pointMapGetConnections(Rcpp::XPtr<PointMap> pointMapPtr) {
  auto &points = pointMapPtr->getPoints();
  int numConnections = 0;
  for (size_t i = 0; i < points.columns(); i++) {
    for (size_t j = 0; j < points.rows(); j++) {
      Point &pnt = points(static_cast<size_t>(j), static_cast<size_t>(i));
      if (pnt.filled() && pnt.hasNode()) {
        PixelRef pix(i, j);
        PixelRefVector connections;
        pnt.getNode().contents(connections);
        numConnections += connections.size();
      }
    }
  }

  Rcpp::IntegerMatrix connectionData(
      numConnections, 2L
  );
  Rcpp::colnames(connectionData) = Rcpp::CharacterVector({
    "from", "to"
  });
  int rowIdx = 0;
  for (size_t i = 0; i < points.columns(); i++) {
    for (size_t j = 0; j < points.rows(); j++) {
      Point &pnt = points(static_cast<size_t>(j), static_cast<size_t>(i));
      if (pnt.filled() && pnt.hasNode()) {
        PixelRef pix(i, j);
        PixelRefVector hood;
        pnt.getNode().contents(hood);
        for (PixelRef &p : hood) {
          const Rcpp::IntegerMatrix::Row &row = connectionData(rowIdx, Rcpp::_);
          row[0] = pix;
          row[1] = p;
          rowIdx++;
        }
      }
    }
  }
  return connectionData;
}

// [[Rcpp::export("Rcpp_PointMap_getFilledPoints")]]
Rcpp::NumericMatrix getFilledPoints(Rcpp::XPtr<PointMap> pointMapPtr) {
  const auto &attrTable = pointMapPtr->getAttributeTable();
  int numCols = attrTable.getNumColumns();
  std::vector<std::string> cellProperties {
    "x", "y", "filled", "blocked", "contextfilled", "edge", "Ref"
  };
  Rcpp::NumericMatrix coordsData(pointMapPtr->getFilledPointCount(),
                                 cellProperties.size() + numCols);
  Rcpp::CharacterVector colNames(cellProperties.size() + numCols);
  {
    int i = 0;
    for(auto prop: cellProperties) {
      colNames[i] = prop;
      ++i;
    }
  }
  for(int i = 0; i < numCols; ++i) {
    colNames[cellProperties.size() + i] = attrTable.getColumnName(i);
  }

  Rcpp::colnames(coordsData) = colNames;
  const auto &points = pointMapPtr->getPoints();

  int rowIdx = 0;
  auto attrRowIt = attrTable.begin();
  for (auto &point: points) {
    if (!point.filled()) continue;
    const Rcpp::NumericMatrix::Row &row = coordsData( rowIdx , Rcpp::_ );
    row[0] = point.getLocation().x;
    row[1] = point.getLocation().y;
    row[2] = point.filled();
    row[3] = point.blocked();
    row[4] = point.contextfilled();
    row[5] = point.edge();
    row[6] = attrRowIt->getKey().value;
    for(int i = 0; i < numCols; ++i) {
      row[cellProperties.size() + i] = attrRowIt->getRow().getValue(i);
    }
    rowIdx++;
    attrRowIt++;
  }
  return coordsData;
}
