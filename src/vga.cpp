// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "salalib/pointdata.h"
#include "salalib/vgamodules/vgametric.h"
#include "salalib/vgamodules/vgavisualglobal.h"
#include "salalib/vgamodules/vgavisuallocal.h"
#include "salalib/vgamodules/vgathroughvision.h"
#include "salalib/vgamodules/vgaangular.h"
#include "salalib/vgamodules/vgaisovist.h"

#include "salalib/vgamodules/vgavisualglobaldepth.h"
#include "salalib/vgamodules/vgametricdepth.h"
#include "salalib/vgamodules/vgaangulardepth.h"

#include "communicator.h"

#include <Rcpp.h>

// [[Rcpp::export("Rcpp_VGA_throughVision")]]
Rcpp::List vgaThroughVision(Rcpp::XPtr<PointMap> pointMapPtr) {
  Rcpp::List result = Rcpp::List::create(
    Rcpp::Named("completed") = false
  );
  auto analysisResult = VGAThroughVision().run(
    getCommunicator(true).get(),
    *pointMapPtr,
    false);
  result["completed"] = analysisResult.completed;
  result["newAttributes"] = analysisResult.getAttributes();
  return result;
}

// [[Rcpp::export("Rcpp_VGA_angular")]]
Rcpp::List vgaAngular(Rcpp::XPtr<PointMap> pointMapPtr,
                      double radius, bool gatesOnly) {

  Rcpp::List result = Rcpp::List::create(
    Rcpp::Named("completed") = false
  );
  auto analysisResult = VGAAngular(radius, gatesOnly)
    .run(getCommunicator(true).get(), *pointMapPtr, false);
  result["completed"] = analysisResult.completed;
  result["newAttributes"] = analysisResult.getAttributes();
  return result;
}

// [[Rcpp::export("Rcpp_VGA_metric")]]
Rcpp::List vgaMetric(Rcpp::XPtr<PointMap> pointMapPtr,
                     double radius, bool gatesOnly) {

  if (radius != -1.0 && radius <= 0) {
    Rcpp::stop("Radius for metric vga must be n (-1) for the whole range or a "
                 "positive number. Got %d", radius);
  }
  Rcpp::List result = Rcpp::List::create(
    Rcpp::Named("completed") = false
  );
  auto analysisResult = VGAMetric(radius, gatesOnly)
    .run(getCommunicator(true).get(), *pointMapPtr, false);
  result["completed"] = analysisResult.completed;
  result["newAttributes"] = analysisResult.getAttributes();
  return result;
}

// [[Rcpp::export("Rcpp_VGA_visualGlobal")]]
Rcpp::List vgaVisualGlobal(Rcpp::XPtr<PointMap> pointMapPtr,
                           int radius, bool gatesOnly) {

  if (radius != -1 && (radius < 1 || radius > 99)) {
    Rcpp::stop("Radius for visibility analysis must be n (-1) for the whole "
                 "range or an integer between 1 and 99 inclusive. Got %i",
                 radius);
  }

  Rcpp::List result = Rcpp::List::create(
    Rcpp::Named("completed") = false
  );
  auto analysisResult = VGAVisualGlobal(radius, gatesOnly)
    .run(getCommunicator(true).get(), *pointMapPtr, false);
  result["completed"] = analysisResult.completed;
  result["newAttributes"] = analysisResult.getAttributes();
  return result;
}

// [[Rcpp::export("Rcpp_VGA_visualLocal")]]
Rcpp::List vgaVisualLocal(Rcpp::XPtr<PointMap> pointMapPtr, bool gatesOnly) {

  Rcpp::List result = Rcpp::List::create(
    Rcpp::Named("completed") = false
  );
  auto analysisResult = VGAVisualLocal(gatesOnly)
    .run(getCommunicator(true).get(), *pointMapPtr, false);
  result["completed"] = analysisResult.completed;
  result["newAttributes"] = analysisResult.getAttributes();
  return result;
}

// [[Rcpp::export("Rcpp_VGA_isovist")]]
Rcpp::List vgaIsovist(Rcpp::XPtr<PointMap> pointMapPtr,
                      Rcpp::XPtr<ShapeMap> shapeMapPtr) {

  Rcpp::List result = Rcpp::List::create(
    Rcpp::Named("completed") = false
  );
  auto shapeMap = shapeMapPtr->getAllShapes();

  std::vector<SalaShape> shapes;
  shapes.reserve(shapeMap.size());
  for(auto it = shapeMap.begin(); it != shapeMap.end(); ++it) {
    shapes.push_back(it->second);
  }

  auto analysisResult = VGAIsovist(shapes)
    .run(getCommunicator(true).get(), *pointMapPtr, false);
  result["completed"] = analysisResult.completed;
  result["newAttributes"] = analysisResult.getAttributes();
  return result;
}

// [[Rcpp::export("Rcpp_VGA_visualDepth")]]
Rcpp::List vgaVisualDepth(Rcpp::XPtr<PointMap> pointMapPtr,
                            Rcpp::NumericMatrix stepDepthPoints) {
  Rcpp::List result = Rcpp::List::create(
    Rcpp::Named("completed") = false
  );

  pointMapPtr->clearSel();
  for (int r = 0; r < stepDepthPoints.rows(); ++r) {
    auto coordRow = stepDepthPoints.row(r);
    Point2f p(coordRow[0], coordRow[1]);
    QtRegion region(p, p);
    pointMapPtr->setCurSel(region, true);
  }
  auto analysisResult = VGAVisualGlobalDepth()
    .run(getCommunicator(true).get(), *pointMapPtr, false);
  pointMapPtr->clearSel();
  result["completed"] = analysisResult.completed;
  result["newAttributes"] = analysisResult.getAttributes();
  return result;
}

// [[Rcpp::export("Rcpp_VGA_metricDepth")]]
Rcpp::List vgaMetricDepth(Rcpp::XPtr<PointMap> pointMapPtr,
                          Rcpp::NumericMatrix stepDepthPoints) {
  Rcpp::List result = Rcpp::List::create(
    Rcpp::Named("completed") = false
  );

  pointMapPtr->clearSel();
  for (int r = 0; r < stepDepthPoints.rows(); ++r) {
    auto coordRow = stepDepthPoints.row(r);
    Point2f p(coordRow[0], coordRow[1]);
    QtRegion region(p, p);
    pointMapPtr->setCurSel(region, true);
  }
  auto analysisResult = VGAMetricDepth()
    .run(getCommunicator(true).get(), *pointMapPtr, false);
  pointMapPtr->clearSel();
  result["completed"] = analysisResult.completed;
  result["newAttributes"] = analysisResult.getAttributes();
  return result;
}

// [[Rcpp::export("Rcpp_VGA_angularDepth")]]
Rcpp::List vgaAngularDepth(Rcpp::XPtr<PointMap> pointMapPtr,
                           Rcpp::NumericMatrix stepDepthPoints) {
  Rcpp::List result = Rcpp::List::create(
    Rcpp::Named("completed") = false
  );

  pointMapPtr->clearSel();
  for (int r = 0; r < stepDepthPoints.rows(); ++r) {
    auto coordRow = stepDepthPoints.row(r);
    Point2f p(coordRow[0], coordRow[1]);
    QtRegion region(p, p);
    pointMapPtr->setCurSel(region, true);
  }
  auto analysisResult = VGAAngularDepth()
    .run(getCommunicator(true).get(), *pointMapPtr, false);
  pointMapPtr->clearSel();
  result["completed"] = analysisResult.completed;
  result["newAttributes"] = analysisResult.getAttributes();
  return result;
}

