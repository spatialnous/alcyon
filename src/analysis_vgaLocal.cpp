// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "salalib/vgamodules/vgavisuallocal.h"
#include "salalib/vgamodules/vgavisuallocalopenmp.h"
#include "salalib/vgamodules/vgavisuallocaladjmatrix.h"
#include "salalib/vgamodules/vgathroughvision.h"

#include "enum_VGALocalAlgorithm.h"

#include "communicator.h"

#include <Rcpp.h>

// [[Rcpp::plugins(openmp)]]

// [[Rcpp::export("Rcpp_VGA_visualLocal")]]
Rcpp::List vgaVisualLocal(Rcpp::XPtr<PointMap> pointMapPtr,
                          const Rcpp::Nullable<bool> gatesOnlyNV = R_NilValue,
                          const Rcpp::Nullable<int> nthreadsNV = R_NilValue,
                          const Rcpp::Nullable<int> algorithmNV = R_NilValue,
                          const Rcpp::Nullable<bool> copyMapNV = R_NilValue) {
  bool gatesOnly = true;
  if (gatesOnlyNV.isNotNull()) {
    gatesOnly = Rcpp::as<bool>(gatesOnlyNV);
  }
  int nthreads = 0;
  if (nthreadsNV.isNotNull()) {
    nthreads = Rcpp::as<bool>(nthreadsNV);
  }
  if (nthreads < 0) {
    Rcpp::stop("Number of threads has to be >= 1 or 0 for all (" +
      std::to_string(nthreads) + " provided)");
  }
  VGALocalAlgorithm algorithm = VGALocalAlgorithm::Standard;
  if (algorithmNV.isNotNull()) {
    algorithm = static_cast<VGALocalAlgorithm>(Rcpp::as<int>(algorithmNV));
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
  if (algorithm == VGALocalAlgorithm::Standard) {
    if (nthreads == 1) {
      // original algorithm
      analysisResult = VGAVisualLocal(gatesOnly)
        .run(getCommunicator(true).get(), *pointMapPtr, false);
    } else {
      // openmp algorithm
      analysisResult = VGAVisualLocalOpenMP(*pointMapPtr, nthreads == 0 ?
                                              std::nullopt :
                                              std::make_optional(nthreads))
        .run(getCommunicator(true).get());
    }
  } else if (algorithm == VGALocalAlgorithm::AdjacencyMatrix) {
    // adjacency matrix algorithm
    analysisResult = VGAVisualLocalAdjMatrix(*pointMapPtr, gatesOnly,
                                          nthreads == 0 ?
                                            std::nullopt :
                                            std::make_optional(nthreads))
      .run(getCommunicator(true).get());
  } else {
    Rcpp::stop("Unknown algorithm provided: " +
      std::to_string(static_cast<int>(algorithm)));
  }

  result["completed"] = analysisResult.completed;
  result["newAttributes"] = analysisResult.getAttributes();
  result["mapPtr"] = pointMapPtr;
  return result;
}

// [[Rcpp::export("Rcpp_VGA_throughVision")]]
Rcpp::List vgaThroughVision(Rcpp::XPtr<PointMap> pointMapPtr,
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
  auto analysisResult = VGAThroughVision().run(
    getCommunicator(true).get(),
    *pointMapPtr,
    false);
  result["completed"] = analysisResult.completed;
  result["newAttributes"] = analysisResult.getAttributes();
  result["mapPtr"] = pointMapPtr;
  return result;
}
