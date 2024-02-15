// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "salalib/shapemap.h"
#include "genlib/p2dpoly.h"

#include <Rcpp.h>

RCPP_EXPOSED_CLASS(ShapeMap);

// [[Rcpp::export("Rcpp_ShapeMap_make")]]
Rcpp::XPtr<ShapeMap> make(std::string name) {
  return Rcpp::XPtr<ShapeMap>(new ShapeMap(name), true);
}

// [[Rcpp::export("Rcpp_ShapeMap_getName")]]
std::string getName(Rcpp::XPtr<ShapeMap> shapeMap) {
  return shapeMap->getName();
}

// [[Rcpp::export("Rcpp_ShapeMap_getAttributeNames")]]
std::vector<std::string> getAttributeNames(Rcpp::XPtr<ShapeMap> shapeMap) {
  std::vector<std::string> names;
  auto &attributes = shapeMap->getAttributeTable();
  int numCols = attributes.getNumColumns();
  // + 1 for the key column
  names.reserve(1 + numCols);
  names.push_back(attributes.getColumnName(size_t(-1)));
  for(int i = 0; i < attributes.getNumColumns(); ++i) {
    names.push_back(attributes.getColumnName(i));
  }
  return names;
}

// [[Rcpp::export("Rcpp_ShapeMap_getAttributeData")]]
std::map<std::string, std::vector<double>> getAttributeData(
    Rcpp::XPtr<ShapeMap> shapeMap,
    std::vector<std::string> attributeNames) {
  auto &attrbs = shapeMap->getAttributeTable();
  std::map<std::string, std::vector<double>> data;
  for (auto &attributeName: attributeNames) {
    auto& attributeData = data[attributeName];
    attributeData.reserve(attrbs.getNumRows());
    if (attributeName == attrbs.getColumnName(size_t(-1))) {
      for(auto rowIt = attrbs.begin(); rowIt != attrbs.end(); ++rowIt) {
        attributeData.push_back(rowIt->getKey().value);
      }
    } else {
      size_t colIdx = attrbs.getColumnIndex(attributeName);
      for(auto rowIt = attrbs.begin(); rowIt != attrbs.end(); ++rowIt) {
        attributeData.push_back(rowIt->getRow().getValue(colIdx));
      }
    }
  }
  return data;
}

// [[Rcpp::export("Rcpp_ShapeMap_getShapesAsLineCoords")]]
Rcpp::NumericMatrix getShapesAsLineCoords(Rcpp::XPtr<ShapeMap> shapeMap) {
  std::vector<std::string> names;
  const auto &lines = shapeMap->getAllShapesAsLines();
  Rcpp::NumericMatrix coords(lines.size(), 4);

  int rowIdx = 0;
  for (auto &line: lines) {
    const Rcpp::NumericMatrix::Row &row = coords( rowIdx , Rcpp::_ );
    row[0] = line.start().x;
    row[1] = line.start().y;
    row[2] = line.end().x;
    row[3] = line.end().y;
    rowIdx++;
  }
  return coords;
}

// [[Rcpp::export("Rcpp_ShapeMap_getShapesAsPolygonCoords")]]
Rcpp::GenericVector getShapesAsPolygonCoords(Rcpp::XPtr<ShapeMap> shapeMap) {
  float TOLERANCE = 0.0001;
  std::vector<std::string> names;
  Rcpp::GenericVector coords;
  const auto &shapes = shapeMap->getAllShapes();

  for (const auto &shape: shapes) {
    if(!shape.second.isPolygon()) continue;
    const auto &firstPoint = *shape.second.m_points.begin();
    const auto &lastPoint = *shape.second.m_points.rbegin();
    bool lastPointIsFirst = fabs(firstPoint.x - lastPoint.x) < TOLERANCE &&
      fabs(firstPoint.y - lastPoint.y) < TOLERANCE;
    Rcpp::NumericMatrix poly(shape.second.m_points.size() +
      (lastPointIsFirst ? 0 : 1), 2);
    int rowIdx = 0;
    for (const auto &point: shape.second.m_points) {
      const Rcpp::NumericMatrix::Row &row = poly( rowIdx , Rcpp::_ );
      row[0] = point.x;
      row[1] = point.y;
      rowIdx++;
    }
    if (!lastPointIsFirst) {
      const Rcpp::NumericMatrix::Row &row = poly( rowIdx , Rcpp::_ );
      row[0] = firstPoint.x;
      row[1] = firstPoint.y;
    }
    coords.push_back(poly);
  }
  return coords;
}
