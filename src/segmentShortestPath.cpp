// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "salalib/shapegraph.h"

#include "salalib/radiustype.h"
#include "salalib/segmmodules/segmmetricshortestpath.h"
#include "salalib/segmmodules/segmtopologicalshortestpath.h"
#include "salalib/segmmodules/segmtulipshortestpath.h"

#include "TraversalType.h"
#include "communicator.h"

#include <Rcpp.h>

// [[Rcpp::export("Rcpp_segmentShortestPath")]]
Rcpp::List segmentShortestPath(Rcpp::XPtr<ShapeGraph> shapeGraph, const int stepType,
                               Rcpp::NumericMatrix origPoints,
                               Rcpp::NumericMatrix destPoints,
                               const Rcpp::Nullable<int> tulipBinsNV = R_NilValue,
                               const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
                               const Rcpp::Nullable<bool> verboseNV = R_NilValue,
                               const Rcpp::Nullable<bool> progressNV = R_NilValue) {
  int tulipBins = 0;
  if (tulipBinsNV.isNotNull()) {
    tulipBins = Rcpp::as<int>(tulipBinsNV);
  }
  bool copyMap = true;
  if (copyMapNV.isNotNull()) {
    copyMap = Rcpp::as<bool>(copyMapNV);
  }
  bool verbose = false;
  if (verboseNV.isNotNull()) {
    verbose = Rcpp::as<bool>(verboseNV);
  }
  bool progress = false;
  if (progressNV.isNotNull()) {
    progress = Rcpp::as<bool>(progressNV);
  }

  if (verbose) {
    Rcpp::Rcout << "ok\nSelecting cells... " << '\n';
  }

  if (origPoints.rows() != destPoints.rows()) {
    Rcpp::stop("Different number of origins and destinations provided (%d %d).",
               origPoints.rows(), destPoints.rows());
  }

  if (copyMap) {
    auto prevShapeGraph = shapeGraph;
    shapeGraph = Rcpp::XPtr(new ShapeGraph());
    shapeGraph->copy(*prevShapeGraph, ShapeMap::COPY_ALL, true);
  }

  std::set<int> origins;
  for (int r = 0; r < origPoints.rows(); ++r) {
    auto coordRow = origPoints.row(r);
    Point2f p(coordRow[0], coordRow[1]);
    auto graphRegion = shapeGraph->getRegion();
    if (!graphRegion.contains(p)) {
      Rcpp::stop("Point outside of target region");
    }
    QtRegion region(p, p);
    origins.insert(shapeGraph->getShapesInRegion(region).begin()->first);
  }

  std::set<int> destinations;
  for (int r = 0; r < destPoints.rows(); ++r) {
    auto coordRow = destPoints.row(r);
    Point2f p(coordRow[0], coordRow[1]);
    auto graphRegion = shapeGraph->getRegion();
    if (!graphRegion.contains(p)) {
      Rcpp::stop("Point outside of target region");
    }
    QtRegion region(p, p);
    destinations.insert(shapeGraph->getShapesInRegion(region).begin()->first);
  }

  if (verbose) {
    Rcpp::Rcout << "ok\nCalculating shortest-paths.. " << '\n';
  }

  Rcpp::List result = Rcpp::List::create(Rcpp::Named("completed") = false);

  try {
    AppendableAnalysisResult analysisResult;
    auto destIt = destinations.begin();
    for (auto &origin: origins) {
      switch (static_cast<TraversalType>(stepType)) {
      case TraversalType::Angular:
        if (tulipBins != 0) {
          analysisResult.append(SegmentTulipShortestPath(
            *shapeGraph, tulipBins, origin, *destIt)
          .run(getCommunicator(progress).get()));
        } else {
          // full angular was never created as a step-function
          // do normal tulip
          Rcpp::stop("Full angular depth not implemented, "
                       "provide tulipBins for quantization");
        }
        break;
      case TraversalType::Metric: {
        analysisResult.append(SegmentMetricShortestPath(
          *shapeGraph, origin, *destIt).run(
          getCommunicator(progress).get()));
        break;
      }
      case TraversalType::Topological: {
        analysisResult.append(SegmentTopologicalShortestPath(
          *shapeGraph, origin, *destIt).run(
          getCommunicator(progress).get()));
        break;
      }
      default: {
        Rcpp::stop("Error, unsupported step type");
      }
      }
      destIt++;
    }
    result["completed"] = analysisResult.completed;
    result["newAttributes"] = analysisResult.getAttributes();
    result["mapPtr"] = shapeGraph;
  } catch (Communicator::CancelledException) {
    // result["completed"] = false;
  }

  return result;
}
