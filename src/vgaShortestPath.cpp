// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "salalib/pointmap.h"
#include "salalib/vgamodules/vgavisualshortestpath.h"
#include "salalib/vgamodules/vgametricshortestpath.h"
#include "salalib/vgamodules/vgametricshortestpathtomany.h"
#include "salalib/vgamodules/vgaangularshortestpath.h"

#include "communicator.h"

#include <Rcpp.h>

// [[Rcpp::export("Rcpp_VGA_visualShortestPath")]]
Rcpp::List vgaVisualShortestPath(
    Rcpp::XPtr<PointMap> pointMapPtr,
    Rcpp::NumericMatrix origPoints,
    Rcpp::NumericMatrix destPoints,
    const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
    const Rcpp::Nullable<bool> verboseNV = R_NilValue,
    const Rcpp::Nullable<bool> progressNV = R_NilValue) {
  bool verbose = false;
  if (verboseNV.isNotNull()) {
    verbose = Rcpp::as<bool>(verboseNV);
  }
  bool progress = false;
  if (progressNV.isNotNull()) {
    progress = Rcpp::as<bool>(progressNV);
  }
  if (origPoints.rows() != destPoints.rows()) {
    Rcpp::stop("Different number of origins and destinations provided (%d %d).",
               origPoints.rows(), destPoints.rows());
  }
  std::set<PixelRef> origins;
  for (int r = 0; r < origPoints.rows(); ++r) {
    auto coordRow = origPoints.row(r);
    Point2f p(coordRow[0], coordRow[1]);
    auto pixref = pointMapPtr->pixelate(p);
    if (!pointMapPtr->includes(pixref)) {
      Rcpp::stop("Origin point (%d %d) outside of target pointmap region.", p.x, p.y);
    }
    if (!pointMapPtr->getPoint(pixref).filled()) {
      Rcpp::stop("Origin point (%d %d) not pointing to a filled cell.", p.x, p.y);
    }
    origins.insert(pixref);
  }
  std::set<PixelRef> destinations;
  for (int r = 0; r < destPoints.rows(); ++r) {
    auto coordRow = destPoints.row(r);
    Point2f p(coordRow[0], coordRow[1]);
    auto pixref = pointMapPtr->pixelate(p);
    if (!pointMapPtr->includes(pixref)) {
      Rcpp::stop("Destination point (%d %d) outside of target pointmap region.", p.x, p.y);
    }
    if (!pointMapPtr->getPoint(pixref).filled()) {
      Rcpp::stop("Destination point (%d %d) not pointing to a filled cell.", p.x, p.y);
    }
    destinations.insert(pixref);
  }
  bool copyMap = true;
  if (copyMapNV.isNotNull()) {
    copyMap = Rcpp::as<bool>(copyMapNV);
  }
  if (copyMap) {
    auto prevPointMap = pointMapPtr;
    const auto &prevRegion = prevPointMap->getRegion();
    pointMapPtr = Rcpp::XPtr(new PointMap(prevRegion));
    pointMapPtr->copy(*prevPointMap, true, true);
  }
  Rcpp::List result = Rcpp::List::create(
    Rcpp::Named("completed") = false
  );

  AppendableAnalysisResult analysisResult;
  auto destIt = destinations.begin();
  for (auto &origin: origins) {
    analysisResult.append(VGAVisualShortestPath(
      *pointMapPtr, origin, *destIt).run(getCommunicator(true).get()));
    destIt++;
  }

  result["completed"] = analysisResult.completed;
  result["newAttributes"] = analysisResult.getAttributes();
  result["mapPtr"] = pointMapPtr;
  return result;
}

// [[Rcpp::export("Rcpp_VGA_metricShortestPath")]]
Rcpp::List vgaMetricShortestPath(
    Rcpp::XPtr<PointMap> pointMapPtr,
    Rcpp::NumericMatrix origPoints,
    Rcpp::NumericMatrix destPoints,
    const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
    const Rcpp::Nullable<bool> verboseNV = R_NilValue,
    const Rcpp::Nullable<bool> progressNV = R_NilValue) {
  bool verbose = false;
  if (verboseNV.isNotNull()) {
    verbose = Rcpp::as<bool>(verboseNV);
  }
  bool progress = false;
  if (progressNV.isNotNull()) {
    progress = Rcpp::as<bool>(progressNV);
  }
  if (origPoints.rows() != destPoints.rows()) {
    Rcpp::stop("Different number of origins and destinations provided (%d %d).",
               origPoints.rows(), destPoints.rows());
  }
  std::set<PixelRef> origins;
  for (int r = 0; r < origPoints.rows(); ++r) {
    auto coordRow = origPoints.row(r);
    Point2f p(coordRow[0], coordRow[1]);
    auto pixref = pointMapPtr->pixelate(p);
    if (!pointMapPtr->includes(pixref)) {
      Rcpp::stop("Origin point (%d %d) outside of target pointmap region.", p.x, p.y);
    }
    if (!pointMapPtr->getPoint(pixref).filled()) {
      Rcpp::stop("Origin point (%d %d) not pointing to a filled cell.", p.x, p.y);
    }
    origins.insert(pixref);
  }
  std::set<PixelRef> destinations;
  for (int r = 0; r < destPoints.rows(); ++r) {
    auto coordRow = destPoints.row(r);
    Point2f p(coordRow[0], coordRow[1]);
    auto pixref = pointMapPtr->pixelate(p);
    if (!pointMapPtr->includes(pixref)) {
      Rcpp::stop("Destination point (%d %d) outside of target pointmap region.", p.x, p.y);
    }
    if (!pointMapPtr->getPoint(pixref).filled()) {
      Rcpp::stop("Destination point (%d %d) not pointing to a filled cell.", p.x, p.y);
    }
    destinations.insert(pixref);
  }
  bool copyMap = true;
  if (copyMapNV.isNotNull()) {
    copyMap = Rcpp::as<bool>(copyMapNV);
  }
  if (copyMap) {
    auto prevPointMap = pointMapPtr;
    const auto &prevRegion = prevPointMap->getRegion();
    pointMapPtr = Rcpp::XPtr(new PointMap(prevRegion));
    pointMapPtr->copy(*prevPointMap, true, true);
  }
  Rcpp::List result = Rcpp::List::create(
    Rcpp::Named("completed") = false
  );

  AppendableAnalysisResult analysisResult;
  auto destIt = destinations.begin();
  for (auto &origin: origins) {
    analysisResult.append(VGAMetricShortestPath(
      *pointMapPtr, std::set<PixelRef>{origin}, *destIt)
                            .run(getCommunicator(true).get()));
    destIt++;
  }

  result["completed"] = analysisResult.completed;
  result["newAttributes"] = analysisResult.getAttributes();
  result["mapPtr"] = pointMapPtr;
  return result;
}

// [[Rcpp::export("Rcpp_VGA_angularShortestPath")]]
Rcpp::List vgaAngularShortestPath(
    Rcpp::XPtr<PointMap> pointMapPtr,
    Rcpp::NumericMatrix origPoints,
    Rcpp::NumericMatrix destPoints,
    const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
    const Rcpp::Nullable<bool> verboseNV = R_NilValue,
    const Rcpp::Nullable<bool> progressNV = R_NilValue) {
  bool verbose = false;
  if (verboseNV.isNotNull()) {
    verbose = Rcpp::as<bool>(verboseNV);
  }
  bool progress = false;
  if (progressNV.isNotNull()) {
    progress = Rcpp::as<bool>(progressNV);
  }
  if (origPoints.rows() != destPoints.rows()) {
    Rcpp::stop("Different number of origins and destinations provided (%d %d).",
               origPoints.rows(), destPoints.rows());
  }
  std::set<PixelRef> origins;
  for (int r = 0; r < origPoints.rows(); ++r) {
    auto coordRow = origPoints.row(r);
    Point2f p(coordRow[0], coordRow[1]);
    auto pixref = pointMapPtr->pixelate(p);
    if (!pointMapPtr->includes(pixref)) {
      Rcpp::stop("Origin point (%d %d) outside of target pointmap region.", p.x, p.y);
    }
    if (!pointMapPtr->getPoint(pixref).filled()) {
      Rcpp::stop("Origin point (%d %d) not pointing to a filled cell.", p.x, p.y);
    }
    origins.insert(pixref);
  }
  std::set<PixelRef> destinations;
  for (int r = 0; r < destPoints.rows(); ++r) {
    auto coordRow = destPoints.row(r);
    Point2f p(coordRow[0], coordRow[1]);
    auto pixref = pointMapPtr->pixelate(p);
    if (!pointMapPtr->includes(pixref)) {
      Rcpp::stop("Destination point (%d %d) outside of target pointmap region.", p.x, p.y);
    }
    if (!pointMapPtr->getPoint(pixref).filled()) {
      Rcpp::stop("Destination point (%d %d) not pointing to a filled cell.", p.x, p.y);
    }
    destinations.insert(pixref);
  }
  bool copyMap = true;
  if (copyMapNV.isNotNull()) {
    copyMap = Rcpp::as<bool>(copyMapNV);
  }
  if (copyMap) {
    auto prevPointMap = pointMapPtr;
    const auto &prevRegion = prevPointMap->getRegion();
    pointMapPtr = Rcpp::XPtr(new PointMap(prevRegion));
    pointMapPtr->copy(*prevPointMap, true, true);
  }
  Rcpp::List result = Rcpp::List::create(
    Rcpp::Named("completed") = false
  );

  AppendableAnalysisResult analysisResult;
  auto destIt = destinations.begin();
  for (auto &origin: origins) {
    analysisResult.append(VGAAngularShortestPath(
      *pointMapPtr, origin, *destIt).run(getCommunicator(true).get()));
    destIt++;
  }
  result["completed"] = analysisResult.completed;
  result["newAttributes"] = analysisResult.getAttributes();
  result["mapPtr"] = pointMapPtr;
  return result;
}
