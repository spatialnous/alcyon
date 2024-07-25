// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "salalib/pointmap.h"
#include "salalib/vgamodules/vgametric.h"
#include "salalib/vgamodules/vgametricopenmp.h"
#include "salalib/vgamodules/vgavisualglobal.h"
#include "salalib/vgamodules/vgavisualglobalopenmp.h"
#include "salalib/vgamodules/vgaangular.h"
#include "salalib/vgamodules/vgaangularopenmp.h"

#include "communicator.h"

#include <Rcpp.h>

// [[Rcpp::plugins(openmp)]]

// [[Rcpp::export("Rcpp_VGA_angular")]]
Rcpp::List vgaAngular(Rcpp::XPtr<PointMap> pointMapPtr,
                      double radius,
                      const Rcpp::Nullable<bool> gatesOnlyNV = R_NilValue,
                      const Rcpp::Nullable<int> nthreadsNV = R_NilValue,
                      const Rcpp::Nullable<bool> copyMapNV = R_NilValue) {
  bool gatesOnly = true;
  if (gatesOnlyNV.isNotNull()) {
    gatesOnly = Rcpp::as<bool>(gatesOnlyNV);
  }
  int nthreads = 0;
  if (nthreadsNV.isNotNull()) {
    nthreads = Rcpp::as<bool>(nthreadsNV);
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
  AnalysisResult analysisResult;
  if (nthreads == 1) {
    // original algorithm
    analysisResult = VGAAngular(radius, gatesOnly)
      .run(getCommunicator(true).get(), *pointMapPtr, false);
  } else {
    // openmp algorithm
    analysisResult = VGAAngularOpenMP(*pointMapPtr, radius, gatesOnly,
                                      nthreads == 0 ?
                                        std::nullopt :
                                        std::make_optional(nthreads))
    .run(getCommunicator(true).get());
  }
  result["completed"] = analysisResult.completed;
  result["newAttributes"] = analysisResult.getAttributes();
  result["mapPtr"] = pointMapPtr;
  return result;
}

// [[Rcpp::export("Rcpp_VGA_metric")]]
Rcpp::List vgaMetric(Rcpp::XPtr<PointMap> pointMapPtr,
                     double radius,
                     const Rcpp::Nullable<bool> gatesOnlyNV = R_NilValue,
                     const Rcpp::Nullable<int> nthreadsNV = R_NilValue,
                     const Rcpp::Nullable<bool> copyMapNV = R_NilValue) {
  if (radius != -1.0 && radius <= 0) {
    Rcpp::stop("Radius for metric vga must be n (-1) for the whole range or a "
                 "positive number. Got %d", radius);
  }
  bool gatesOnly = true;
  if (gatesOnlyNV.isNotNull()) {
    gatesOnly = Rcpp::as<bool>(gatesOnlyNV);
  }
  int nthreads = 0;
  if (nthreadsNV.isNotNull()) {
    nthreads = Rcpp::as<bool>(nthreadsNV);
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
  AnalysisResult analysisResult;
  if (nthreads == 1) {
    // original algorithm
    analysisResult = VGAMetric(radius, gatesOnly)
      .run(getCommunicator(true).get(), *pointMapPtr, false);
  } else {
    // openmp algorithm
    analysisResult = VGAMetricOpenMP(*pointMapPtr, radius, gatesOnly,
                                      nthreads == 0 ?
                                        std::nullopt :
                                        std::make_optional(nthreads))
    .run(getCommunicator(true).get());
  }
  result["completed"] = analysisResult.completed;
  result["newAttributes"] = analysisResult.getAttributes();
  result["mapPtr"] = pointMapPtr;
  return result;
}

// [[Rcpp::export("Rcpp_VGA_visualGlobal")]]
Rcpp::List vgaVisualGlobal(Rcpp::XPtr<PointMap> pointMapPtr,
                           int radius,
                           const Rcpp::Nullable<bool> gatesOnlyNV = R_NilValue,
                           const Rcpp::Nullable<int> nthreadsNV = R_NilValue,
                           const Rcpp::Nullable<bool> copyMapNV = R_NilValue) {
  if (radius != -1 && (radius < 1 || radius > 99)) {
    Rcpp::stop("Radius for visibility analysis must be n (-1) for the whole "
                 "range or an integer between 1 and 99 inclusive. Got %i",
                 radius);
  }
  bool gatesOnly = true;
  if (gatesOnlyNV.isNotNull()) {
    gatesOnly = Rcpp::as<bool>(gatesOnlyNV);
  }
  int nthreads = 0;
  if (nthreadsNV.isNotNull()) {
    nthreads = Rcpp::as<bool>(nthreadsNV);
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
  AnalysisResult analysisResult;
  if (nthreads == 1) {
    // original algorithm
    analysisResult = VGAVisualGlobal(radius, gatesOnly)
    .run(getCommunicator(true).get(), *pointMapPtr, false);
  } else {
    // openmp algorithm
    analysisResult = VGAVisualGlobalOpenMP(*pointMapPtr, radius, gatesOnly,
                                     nthreads == 0 ?
                                       std::nullopt :
                                       std::make_optional(nthreads))
    .run(getCommunicator(true).get());
  }
  result["completed"] = analysisResult.completed;
  result["newAttributes"] = analysisResult.getAttributes();
  result["mapPtr"] = pointMapPtr;
  return result;
}

