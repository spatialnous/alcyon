// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "salalib/pointmap.h"
#include "salalib/vgamodules/vgavisualglobaldepth.h"
#include "salalib/vgamodules/vgametricdepth.h"
#include "salalib/vgamodules/vgaangulardepth.h"

#include "communicator.h"

#include <Rcpp.h>

// [[Rcpp::export("Rcpp_VGA_visualDepth")]]
Rcpp::List vgaVisualDepth(Rcpp::XPtr<PointMap> pointMapPtr,
                          Rcpp::NumericMatrix stepDepthPoints,
                          const Rcpp::Nullable<bool> copyMapNV = R_NilValue) {
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

  std::set<PixelRef> origins;
  for (int r = 0; r < stepDepthPoints.rows(); ++r) {
    auto coordRow = stepDepthPoints.row(r);
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
  auto analysisResult = VGAVisualGlobalDepth(origins)
    .run(getCommunicator(true).get(), *pointMapPtr, false);
  result["completed"] = analysisResult.completed;
  result["newAttributes"] = analysisResult.getAttributes();
  result["mapPtr"] = pointMapPtr;
  return result;
}

// [[Rcpp::export("Rcpp_VGA_metricDepth")]]
Rcpp::List vgaMetricDepth(Rcpp::XPtr<PointMap> pointMapPtr,
                          Rcpp::NumericMatrix stepDepthPoints,
                          const Rcpp::Nullable<bool> copyMapNV = R_NilValue) {
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

  std::set<PixelRef> origins;
  for (int r = 0; r < stepDepthPoints.rows(); ++r) {
    auto coordRow = stepDepthPoints.row(r);
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
  auto analysisResult = VGAMetricDepth(origins)
    .run(getCommunicator(true).get(), *pointMapPtr, false);
  result["completed"] = analysisResult.completed;
  result["newAttributes"] = analysisResult.getAttributes();
  result["mapPtr"] = pointMapPtr;
  return result;
}

// [[Rcpp::export("Rcpp_VGA_angularDepth")]]
Rcpp::List vgaAngularDepth(Rcpp::XPtr<PointMap> pointMapPtr,
                           Rcpp::NumericMatrix stepDepthPoints,
                           const Rcpp::Nullable<bool> copyMapNV = R_NilValue) {
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

  std::set<PixelRef> origins;
  for (int r = 0; r < stepDepthPoints.rows(); ++r) {
    auto coordRow = stepDepthPoints.row(r);
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
  auto analysisResult = VGAAngularDepth(origins)
    .run(getCommunicator(true).get(), *pointMapPtr, false);
  result["completed"] = analysisResult.completed;
  result["newAttributes"] = analysisResult.getAttributes();
  result["mapPtr"] = pointMapPtr;
  return result;
}
