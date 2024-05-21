// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "salalib/isovist.h"
#include "salalib/shapemap.h"

#include <Rcpp.h>
#include <memory>

bool makeBSPtree(Communicator *communicator,
                 BSPNode *bspRoot,
                 ShapeMap &boundsMap) {

  std::vector<TaggedLine> partitionlines;

  auto refShapes = boundsMap.getAllShapes();
  int k = -1;
  for (const auto &refShape : refShapes) {
    k++;
    std::vector<Line> newLines = refShape.second.getAsLines();
    // I'm not sure what the tagging was meant for any more,
    // tagging at the moment tags the *polygon* it was original attached to
    // must check it is not a zero length line:
    for (const Line &line : newLines) {
      if (line.length() > 0.0) {
        partitionlines.push_back(TaggedLine(line, k));
      }
    }
  }

  if (partitionlines.size()) {

    time_t atime = 0;
    if (communicator) {
      communicator->CommPostMessage(Communicator::NUM_RECORDS,
                                    partitionlines.size());
      qtimer(atime, 0);
    }

    try {
      BSPTree::make(communicator, atime, partitionlines, bspRoot);
      return true;
    } catch (Communicator::CancelledException) {
      // cancelled, do nothing. bspRoot is cleaned by caller
      return false;
    }
  }

  return false;
}


std::set<std::string> setIsovistData(Isovist &isovist, AttributeTable &table,
                                     AttributeRow &row, bool simple_version) {
  std::set<std::string> newAttributes;
  auto [centroid, area] = isovist.getCentroidArea();
  auto [driftmag, driftang] = isovist.getDriftData();
  double perimeter = isovist.getPerimeter();

  std::string colText = "Isovist Area";
  int col = table.getOrInsertColumn(colText);
  newAttributes.insert(colText);
  row.setValue(col, float(area));

  if (!simple_version) {
    colText = "Isovist Compactness";
    col = table.getOrInsertColumn(colText);
    newAttributes.insert(colText);
    row.setValue(col, float(4.0 * M_PI * area / (perimeter * perimeter)));

    colText = "Isovist Drift Angle";
    col = table.getOrInsertColumn(colText);
    newAttributes.insert(colText);
    row.setValue(col, float(180.0 * driftang / M_PI));

    colText = "Isovist Drift Magnitude";
    col = table.getOrInsertColumn(colText);
    newAttributes.insert(colText);
    row.setValue(col, float(driftmag));

    colText = "Isovist Min Radial";
    col = table.getOrInsertColumn(colText);
    newAttributes.insert(colText);
    row.setValue(col, float(isovist.getMinRadial()));

    colText = "Isovist Max Radial";
    col = table.getOrInsertColumn(colText);
    newAttributes.insert(colText);
    row.setValue(col, float(isovist.getMaxRadial()));

    colText = "Isovist Occlusivity";
    col = table.getOrInsertColumn(colText);
    newAttributes.insert(colText);
    row.setValue(col, float(isovist.getOccludedPerimeter()));

    colText = "Isovist Perimeter";
    col = table.getOrInsertColumn(colText);
    newAttributes.insert(colText);
    row.setValue(col, float(perimeter));
  }
  return newAttributes;
}

// [[Rcpp::export("Rcpp_makeIsovists")]]
Rcpp::XPtr<ShapeMap> makeIsovists(Rcpp::XPtr<ShapeMap> boundsMap,
                                  Rcpp::NumericMatrix pointCoords,
                                  Rcpp::NumericVector directionAngles,
                                  Rcpp::NumericVector fieldOfViewAngles,
                                  bool simple_version) {
  if (pointCoords.rows() == 0) {
    Rcpp::stop("No data provided in point coordinates matrix");
  }
  if (directionAngles.size() == 0) {
    Rcpp::stop("No data provided in direction angle vector");
  }
  if (fieldOfViewAngles.size() == 0) {
    Rcpp::stop("No data provided in field-of-view angle vector");
  }
  if (directionAngles.size() == 1) {
    auto directionAngle = *directionAngles.begin();
    directionAngles = Rcpp::NumericVector(pointCoords.rows(), directionAngle);
  }
  if (directionAngles.size() != pointCoords.rows()) {
    Rcpp::stop("The number of direction angles provided is not the "
                 "same as the number of points");
  }
  if (fieldOfViewAngles.size() == 1) {
    auto fieldOfViewAngle = *fieldOfViewAngles.begin();
    fieldOfViewAngles = Rcpp::NumericVector(pointCoords.rows(),
                                            fieldOfViewAngle);
  }
  if (fieldOfViewAngles.size() != pointCoords.rows()) {
    Rcpp::stop("The number of field-of-view angles provided is not the "
                 "same as the number of points");
  }
  std::unique_ptr<BSPNode> bspRoot(new BSPNode());

  Rcpp::XPtr<ShapeMap> map(new ShapeMap("Isovists"));
  if (makeBSPtree(nullptr, bspRoot.get(), *boundsMap)) {
    for (int r = 0; r < pointCoords.rows(); ++r) {
      Isovist iso;
      auto coordRow = pointCoords.row(r);
      Point2f p(coordRow[0], coordRow[1]);

      auto directionAngle = directionAngles[r];
      auto fieldOfViewAngle = fieldOfViewAngles[r];
      double leftAngle = directionAngle - 0.5 * fieldOfViewAngle;
      if (leftAngle < 0 ) {
        leftAngle += 2 * M_PI;
      }

      auto rightAngle = directionAngle + 0.5 * fieldOfViewAngle;
      if (rightAngle > 2 * M_PI) {
        rightAngle -= 2 * M_PI;
      }
      iso.makeit(bspRoot.get(), p, boundsMap->getRegion(),
                 leftAngle, rightAngle);

      // false: closed polygon, true: isovist
      int polyref = map->makePolyShape(iso.getPolygon(), false);
      map->getAllShapes()[polyref].setCentroid(p);

      AttributeTable &table = map->getAttributeTable();
      AttributeRow &row = table.getRow(AttributeKey(polyref));
      setIsovistData(iso, table, row, simple_version);
    }
  }
  return map;
}
