// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "salalib/shapegraph.h"

#include "salalib/radiustype.h"
#include "salalib/segmmodules/segmmetricpd.h"
#include "salalib/segmmodules/segmtopologicalpd.h"
#include "salalib/segmmodules/segmtulipdepth.h"

#include "TraversalType.h"
#include "communicator.h"

#include <Rcpp.h>

// [[Rcpp::export("Rcpp_segmentStepDepth")]]
Rcpp::List segmentStepDepth(Rcpp::XPtr<ShapeGraph> shapeGraph, const int stepType,
                            const std::vector<double> stepDepthPointsX,
                            const std::vector<double> stepDepthPointsY,
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

  if (copyMap) {
    auto prevShapeGraph = shapeGraph;
    shapeGraph = Rcpp::XPtr(new ShapeGraph());
    shapeGraph->copy(*prevShapeGraph, ShapeMap::COPY_ALL, true);
  }

  std::set<int> origins;
  for (int i = 0; i < stepDepthPointsX.size(); ++i) {
    Point2f p2f(stepDepthPointsX[i], stepDepthPointsY[i]);
    auto graphRegion = shapeGraph->getRegion();
    if (!graphRegion.contains(p2f)) {
      Rcpp::stop("Point outside of target region");
    }
    QtRegion r(p2f, p2f);
    origins.insert(shapeGraph->getShapesInRegion(r).begin()->first);
  }

  if (verbose) {
    Rcpp::Rcout << "ok\nCalculating step-depth... " << '\n';
  }

  Rcpp::List result = Rcpp::List::create(Rcpp::Named("completed") = false);

  try {
    AnalysisResult analysisResult;
    switch (static_cast<TraversalType>(stepType)) {
    case TraversalType::Angular:
      if (tulipBins != 0) {
        analysisResult = SegmentTulipDepth(tulipBins, origins)
                             .run(getCommunicator(progress).get(), *shapeGraph,
                                  false /* simple mode */
                             );
      } else {
        // full angular was never created as a step-function
        // do normal tulip
        Rcpp::stop("Full angular depth not implemented, "
                   "provide tulipBins for quantization");
      }
      break;
    case TraversalType::Metric: {
      analysisResult = SegmentMetricPD(origins).run(
          getCommunicator(progress).get(), *shapeGraph, false /* simple mode */
      );
      break;
    }
    case TraversalType::Topological: {
      analysisResult = SegmentTopologicalPD(origins).run(
          getCommunicator(progress).get(), *shapeGraph, false /* simple mode */
      );
      break;
    }
    default: {
      Rcpp::stop("Error, unsupported step type");
    }
    }
    result["completed"] = analysisResult.completed;
    result["newAttributes"] = analysisResult.getAttributes();
    result["mapPtr"] = shapeGraph;
  } catch (Communicator::CancelledException) {
    // result["completed"] = false;
  }

  return result;
}
